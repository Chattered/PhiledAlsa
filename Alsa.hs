{-# LANGUAGE ForeignFunctionInterface, GeneralizedNewtypeDeriving #-}

module Alsa where

import           Control.Applicative
import           Control.Exception
import           Control.Monad.Cont
import           Control.Monad.Reader
import           Foreign.Ptr
import           Foreign.C.String
import           Foreign.C.Types
import qualified Foreign.Marshal.Alloc as C
import           Foreign.Storable

data MixerTag
data ElemTag

type MixerPtr = Ptr MixerTag
type ElemPtr  = Ptr ElemTag

newtype Elem  = Elem ElemPtr

foreign import ccall "snd_mixer_open"                            openMixerIO
  :: Ptr MixerPtr -> CInt -> IO CInt
foreign import ccall "snd_mixer_close"                           closeMixerIO
  :: MixerPtr -> IO CInt
foreign import ccall "snd_mixer_first_elem"                      firstElemIO
  :: MixerPtr -> IO ElemPtr
foreign import ccall "snd_mixer_elem_next"                       nextElemIO
  :: ElemPtr -> IO ElemPtr
foreign import ccall "snd_mixer_selem_get_name"                  elemNameIO
  :: ElemPtr -> IO CString
foreign import ccall "snd_mixer_get_count"                       countElemsIO
  :: MixerPtr -> IO CUInt
foreign import ccall "snd_mixer_attach"                          attachIO
  :: MixerPtr -> CString -> IO CInt
foreign import ccall "snd_mixer_selem_register"                  registerIO
  :: MixerPtr -> Ptr a -> Ptr a -> IO CInt
foreign import ccall "snd_mixer_load"                            loadIO
  :: MixerPtr -> IO CInt
foreign import ccall "snd_mixer_selem_get_playback_volume"       getVolumeIO
  :: ElemPtr -> CInt -> Ptr CLong -> IO CInt
foreign import ccall "snd_mixer_selem_get_playback_volume_range" getVolumeRangeIO
  :: ElemPtr -> Ptr CLong -> Ptr CLong -> IO CInt
foreign import ccall "snd_mixer_selem_set_playback_volume"       setVolumeIO
  :: ElemPtr -> CInt -> CLong -> IO CInt
foreign import ccall "snd_mixer_selem_set_playback_switch"       setSwitchIO
  :: ElemPtr -> CInt -> CInt -> IO CInt
foreign import ccall "snd_strerror"                              strErrorIO
  :: CInt -> IO CString
openMixer      :: MonadIO m => Ptr MixerPtr -> CInt -> m CInt
closeMixer     :: MonadIO m => MixerPtr -> m CInt
firstElem      :: MonadIO m => MixerPtr -> m ElemPtr
nextElem       :: MonadIO m => ElemPtr -> m ElemPtr
elemName'      :: MonadIO m => ElemPtr -> m CString
countElems     :: MonadIO m => MixerPtr -> m CUInt
attach         :: MonadIO m => MixerPtr -> CString -> m CInt
register       :: MonadIO m => MixerPtr -> m CInt
load           :: MonadIO m => MixerPtr -> m CInt
getVolume      :: MonadIO m => ElemPtr -> CInt -> Ptr CLong -> m CInt
getVolumeRange :: MonadIO m => ElemPtr -> Ptr CLong -> Ptr CLong -> m CInt
setVolume'     :: MonadIO m => ElemPtr -> CInt -> CLong -> m CInt
setSwitch'     :: MonadIO m => ElemPtr -> CInt -> CInt -> m CInt
strError       :: MonadIO m => CInt -> m CString
openMixer x y                     = liftIO $ openMixerIO x y
closeMixer                        = liftIO . closeMixerIO
firstElem                         = liftIO . firstElemIO
nextElem                          = liftIO . nextElemIO
elemName'                         = liftIO . elemNameIO
countElems                        = liftIO . countElemsIO
attach m str                      = liftIO $ attachIO m str
register m                        = liftIO $ registerIO m nullPtr nullPtr
load                              = liftIO . loadIO
getVolume elem n volOut           = liftIO $ getVolumeIO elem n volOut
getVolumeRange elem minOut maxOut = liftIO $ getVolumeRangeIO elem minOut maxOut
setVolume' elem n v               = liftIO $ setVolumeIO elem n v
setSwitch' elem n v               = liftIO $ setSwitchIO elem n v
strError                          = liftIO . strErrorIO

newtype Alsa a = Alsa { getAlsa :: ReaderT MixerPtr IO a }
                 deriving (Applicative, Functor, Monad)

safeAlsa :: MonadIO m => CInt -> m ()
safeAlsa 0     = return ()
safeAlsa errNo = liftIO $ throwIO . userError =<< peekCString =<< strError errNo

alloca :: Storable a => ContT r IO (Ptr a)
alloca = ContT C.alloca

getMixer :: String -> IO MixerPtr
getMixer name = flip runContT return $ do
  mptr  <- alloca
  safeAlsa =<< openMixer mptr 0
  mixer <- liftIO $ peek mptr
  safeAlsa <=< attach mixer <=< liftIO . newCString $ name
  safeAlsa <=< register $ mixer
  safeAlsa <=< load     $ mixer
  return mixer

elemName :: Elem -> Alsa String
elemName (Elem e) = do
  Alsa . liftIO $ peekCString <=< elemName' $ e

elems :: Alsa [Elem]
elems = Alsa . ReaderT $ \mixer -> loop =<< firstElem mixer
  where loop e | e == nullPtr = return []
               | otherwise    = do (Elem e :) <$> (loop =<< nextElem e)

get1 :: Storable a => (Ptr a -> IO CInt) -> IO a
get1 f = flip runContT return $ do
  v <- alloca
  safeAlsa =<< (liftIO . f $ v)
  liftIO $ peek v

get2 :: (Storable a, Storable b) => (Ptr a -> Ptr b -> IO CInt) -> IO (a, b)
get2 f = flip runContT return $ do
  (u,v) <- liftM2 (,) alloca alloca
  safeAlsa =<< (liftIO $ f u v)
  liftM2 (,) (liftIO . peek $ u) (liftIO . peek $ v)

volumeRange :: Elem -> Alsa (Integer, Integer)
volumeRange (Elem e) = Alsa . liftIO $ do
  (x,y) <- get2 (getVolumeRange e)
  return (fromIntegral x, fromIntegral y)

volume :: Elem -> Integer -> Alsa Integer
volume (Elem e) n =
  Alsa . liftIO $ fromIntegral <$> get1 (getVolume e $ fromIntegral n)

setVolume :: Elem -> Integer -> Integer -> Alsa ()
setVolume (Elem e) n v =
  Alsa $ safeAlsa =<< setVolume' e (fromIntegral n) (fromIntegral v)

switchOff :: Elem -> Integer -> Alsa ()
switchOff (Elem e) n =
  Alsa $ safeAlsa =<< setSwitch' e (fromIntegral n) 0

switchOn :: Elem -> Integer -> Alsa ()
switchOn (Elem e) n =
  Alsa $ safeAlsa =<< setSwitch' e (fromIntegral n) 1

runAlsa :: String -> Alsa a -> IO a
runAlsa name (Alsa alsa) = do
  mixer <- getMixer name
  x <- runReaderT alsa mixer
  safeAlsa =<< closeMixer mixer
  return x
