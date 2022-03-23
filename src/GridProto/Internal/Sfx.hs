module GridProto.Internal.Sfx where

import qualified SDL.Mixer as Mixer
import Data.IORef

import GridProto.Internal.Sfx.Success
import GridProto.Internal.Sfx.Bell
import GridProto.Internal.Sfx.Select
import GridProto.Internal.Sfx.NoSelect
import GridProto.Internal.Sfx.Scroll
import GridProto.Internal.Sfx.Chimes
import GridProto.Internal.Sfx.Laser
import GridProto.Internal.Sfx.PowerUp
import GridProto.Internal.Sfx.Jump
import GridProto.Internal.Sfx.Damage
import GridProto.Internal.Sfx.Explosion
import GridProto.Internal.Sfx.Noise

data Sfx
  = SfxSuccess
  | SfxBell
  | SfxSelect
  | SfxNoSelect
  | SfxScroll
  | SfxChimes
  | SfxLaser
  | SfxPowerUp
  | SfxJump
  | SfxDamage
  | SfxExplosion
  | SfxNoise
  deriving (Show, Eq, Bounded, Enum)

data SfxData = SfxData
  { sfxSuccess
  , sfxBell
  , sfxSelect
  , sfxNoSelect
  , sfxScroll
  , sfxChimes
  , sfxLaser
  , sfxPowerUp
  , sfxJump
  , sfxDamage
  , sfxExplosion
  , sfxNoise
  :: Mixer.Chunk
  }

loadSfxData :: IO SfxData
loadSfxData = SfxData
  <$> Mixer.decode sfxSuccessData
  <*> Mixer.decode sfxBellData
  <*> Mixer.decode sfxSelectData
  <*> Mixer.decode sfxNoSelectData
  <*> Mixer.decode sfxScrollData
  <*> Mixer.decode sfxChimesData
  <*> Mixer.decode sfxLaserData
  <*> Mixer.decode sfxPowerUpData
  <*> Mixer.decode sfxJumpData
  <*> Mixer.decode sfxDamageData
  <*> Mixer.decode sfxExplosionData
  <*> Mixer.decode sfxNoiseData

freeSfxData :: SfxData -> IO ()
freeSfxData sd = do
  Mixer.free $ sfxSuccess sd
  Mixer.free $ sfxBell sd
  Mixer.free $ sfxSelect sd
  Mixer.free $ sfxNoSelect sd
  Mixer.free $ sfxScroll sd
  Mixer.free $ sfxChimes sd
  Mixer.free $ sfxLaser sd
  Mixer.free $ sfxPowerUp sd
  Mixer.free $ sfxJump sd
  Mixer.free $ sfxDamage sd
  Mixer.free $ sfxExplosion sd
  Mixer.free $ sfxNoise sd

playSfxs :: SfxData -> IORef [Int] -> [Sfx] -> IO ()
playSfxs sd chanRef sfxs = flip mapM_ sfxs $ \sfx -> case sfx of
  SfxSuccess -> play sfxSuccess
  SfxBell -> play sfxBell
  SfxSelect -> play sfxSelect
  SfxNoSelect -> play sfxNoSelect
  SfxScroll -> play sfxScroll
  SfxChimes -> play sfxChimes
  SfxLaser -> play sfxLaser
  SfxPowerUp -> play sfxPowerUp
  SfxJump -> play sfxJump
  SfxDamage -> play sfxDamage
  SfxExplosion -> play sfxExplosion
  SfxNoise -> play sfxNoise
  where
    play f = do
      chans <- readIORef chanRef
      writeIORef chanRef (tail chans)
      Mixer.playOn (fromIntegral $ head chans) 1 (f sd)
