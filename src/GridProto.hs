module GridProto
  ( GridProto(..)
  , runGridProto
  , defaultGridProto
  --
  , Map, fromList, lookupMap, (!), delete, alter, insert, filterWithKey, member, notMember, toList
  , Color(..)
  , Shape(..)
  , Input(..)
  , Controller(..)
  , Axis(..)
  , Mouse(..)
  , Key(..)
  , KeyState(..)
  , Keys(..)
  , Tile(..)
  , Sfx(..)
  , Viewport(..)
  , Viewports
  , View
  , MapTile(..)
  , emptyView
  , lookupKey
  , placeTilesAt
  , mergeTiles
  , mergeViewport
  , mergeViewports
  , shade, tint
  , rd0, rd1, rd2
  , or0, or1, or2
  , yw0, yw1, yw2
  , ch0, ch1, ch2
  , gn0, gn1, gn2
  , sp0, sp1, sp2
  , cn0, cn1, cn2
  , az0, az1, az2
  , bu0, bu1, bu2
  , vt0, vt1, vt2
  , mg0, mg1, mg2
  , rs0, rs1, rs2
  , br0, br1, br2
  , gy0, gy1, gy2
  , wh0, wh1, wh2
  , bk0, bk1, bk2
  , rainbow
  , warms, cools
  , colorValue
  , sdlColor
  , colorWheel0, colorWheel1, colorWheel2
  ) where

import Data.Map (Map, fromList, (!), delete, alter, insert, filterWithKey, member, notMember, toList)
import GridProto.Internal.Runner
import GridProto.Internal.Core
import GridProto.Internal.Sfx
