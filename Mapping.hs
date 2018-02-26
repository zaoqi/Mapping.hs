--    Copyright (C) 2018  Zaoqi

--    This program is free software: you can redistribute it and/or modify
--    it under the terms of the GNU Affero General Public License as published
--    by the Free Software Foundation, either version 3 of the License, or
--    (at your option) any later version.

--    This program is distributed in the hope that it will be useful,
--    but WITHOUT ANY WARRANTY; without even the implied warranty of
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--    GNU Affero General Public License for more details.

--    You should have received a copy of the GNU Affero General Public License
--    along with this program.  If not, see <http://www.gnu.org/licenses/>.
module Mapping(
    Mapping(MappingNil),
    mappingRef,
    mappingSet,
    mappingUnion,
    mappingMayRemove,
    mappingRemove,
    mappingHas,
    mappingToList,
    listToMapping,
    mappingAppendList,
    ) where

type List a = [a]

data Mapping k v = MappingNil | MappingNode (Mapping k v) k v (Mapping k v) deriving Show

mappingRef :: Ord k => Mapping k v -> k -> v -> v
mappingRef (MappingNode l mk v r) k d =
    case compare k mk of
        LT -> mappingRef l k d
        EQ -> v
        GT -> mappingRef r k d
mappingRef MappingNil _ d = d

mappingSet :: Ord k => Mapping k v -> k -> v -> Mapping k v
mappingSet (MappingNode l mk mv r) k v =
    case compare k mk of
        LT -> MappingNode (mappingSet l k v) mk mv r
        EQ -> MappingNode l mk v r
        GT -> MappingNode l mk mv (mappingSet r k v)
mappingSet MappingNil k v = MappingNode MappingNil k v MappingNil

mappingUnion :: Ord k => (k -> v -> v -> v) -> Mapping k v -> Mapping k v -> Mapping k v
mappingUnion combine (MappingNode l1 k1 v1 r1) (MappingNode l2 k2 v2 r2) =
    let
        l = mappingUnion combine l1 l2
        r = mappingUnion combine r1 r2
    in
        case compare k1 k2 of
            LT -> mappingUnion combine r l
            EQ -> MappingNode l k1 (combine k1 v1 v2) r
            GT -> mappingUnion combine l r
mappingUnion _ MappingNil x = x
mappingUnion _ x MappingNil = x

mappingMayRemove :: Ord k => Mapping k v -> k -> Mapping k v
mappingMayRemove (MappingNode l mk v r) k =
    case compare k mk of
        LT -> MappingNode (mappingMayRemove l k) mk v r
        EQ -> mappingUnion (\k v1 v2 -> error "!") l r
        GT -> MappingNode l mk v (mappingMayRemove r k)
mappingMayRemove MappingNil _ = MappingNil

mappingRemove :: Ord k => Mapping k v -> k -> Maybe (Mapping k v)
mappingRemove (MappingNode l mk v r) k =
    case compare k mk of
        LT -> fmap (\l -> MappingNode l mk v r) (mappingRemove l k)
        EQ -> Just (mappingUnion (\k v1 v2 -> error "!") l r)
        GT -> fmap (\r -> MappingNode l mk v r) (mappingRemove r k)
mappingRemove MappingNil _ = Nothing

mappingHas :: Ord k => Mapping k v -> k -> Bool
mappingHas (MappingNode l mk v r) k =
    case compare k mk of
        LT -> mappingHas l k
        EQ -> True
        GT -> mappingHas r k
mappingHas MappingNil _ = False

mappingToList :: Mapping k v -> List (k, v)
mappingToList (MappingNode l k v r) = (mappingToList l) ++ ((k, v) : mappingToList r)
mappingToList MappingNil = []

listToMapping :: Ord k => List (k, v) -> Mapping k v
listToMapping xs = mappingAppendList MappingNil xs

mappingAppendList :: Ord k => Mapping k v -> List (k, v) -> Mapping k v
mappingAppendList m xs = foldl (\h (k, v) -> mappingSet h k v) m xs

instance (Eq k, Eq v) => Eq (Mapping k v) where
    x == y = mappingToList x == mappingToList y

instance (Ord k, Ord v) => Ord (Mapping k v) where
    compare x y = compare (mappingToList x) (mappingToList y)

