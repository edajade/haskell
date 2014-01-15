-- The OpenCog AtomSpace is a graph database / knowledge base. The official implementation is in C++. This is a simplified version in Haskell

import Data.Map as Map
import Data.Set as Set
import Data.List as List
import Data.Maybe

import Debug.Trace as Trace
import System.Environment

--newtype Handle = Handle Integer
type Type = Int
type TypeName = String
type Name = String

type Strength = Float
type Count = Float

data TruthValue = NoTV | 
    SimpleTV { mean :: Strength, count :: Count } |
    IndefiniteTV { a :: Strength, b :: Strength}
        deriving (Show, Read, Eq, Ord)

data AttentionValue = NoAV |
     AttentionValue { sti :: Int, lti :: Int} deriving (Show, Read, Eq, Ord)

data AtomIdentity = AtomIdentity {
        typeCode :: Type,
        name :: Name,
        out :: [AtomIdentity]
    } deriving (Eq, Ord, Show)

node typeCode name = AtomIdentity { typeCode = typeCode, name = name, out = [] }
link typeCode out = AtomIdentity { typeCode = typeCode, name = "", out = out }

data AtomStats = AtomStats {
    av :: AttentionValue,
    tv :: TruthValue
    } deriving (Eq, Ord, Show)

type AtomSet = Set.Set AtomIdentity
type IncomingMapType = Map.Map AtomIdentity AtomSet
data AtomSpace = AtomSpace {
    atomIdentities :: AtomSet,
    atomStats :: Map.Map AtomIdentity AtomStats,
    incomingMap :: IncomingMapType
    -- Later it would be easy to make this more generic
    -- typeIndex :: Map.Map Type AtomIdentity
    } deriving (Show)

emptyAtomSpace = AtomSpace {
    atomIdentities = Set.empty,
    atomStats = Map.empty,
    incomingMap = Map.empty
    }

defaultAtomStats = AtomStats { av = NoAV, tv = NoTV }

modifyAtomIdentities :: (Set.Set AtomIdentity -> Set.Set AtomIdentity) -> AtomSpace -> AtomSpace
modifyAtomIdentities modify atomSpace =
    atomSpace {
        atomIdentities = modify $ atomIdentities atomSpace
    }

addIncomingLinks :: AtomIdentity -> IncomingMapType -> IncomingMapType
addIncomingLinks parent inMap =
    let
        children = out parent
        alterIncomingSet incoming = Just $ Set.insert parent incoming
    in 
        foldr (Map.update (alterIncomingSet)) inMap children

addListToSet set list = foldr (Set.insert) set list

-- This is not recursive and must be used from the bottom up
addAtomInternal :: AtomIdentity -> AtomSpace -> AtomSpace
addAtomInternal identity atomSpace = 
        let
            atomSpace' = atomSpace {
                atomIdentities = Set.insert identity $ atomIdentities atomSpace,
                incomingMap = Map.insert identity Set.empty  (incomingMap atomSpace),
                atomStats = Map.insert identity defaultAtomStats (atomStats atomSpace)
                } 
            --addParentToChildsIncomingSet parent child = 
            --outMap = (recursiveOutMap identity)
        in
            atomSpace'
            --atomSpace {
            --    incoming = modifyIncoming inIndex (recursiveOutMap identity)
            --}

-- Public function. If you call it with a nested atom, it will add recursively
addAtom :: AtomSpace -> AtomIdentity -> AtomSpace
addAtom atomSpace identity =
    let
        -- add atoms to the basic indexes (if not already there).
        maybeAddAtom id atoms = if containsAtom atoms id then atoms else addAtomInternal id atoms
        atomSpace' = foldr maybeAddAtom atomSpace $ preorder identity

        -- extend the incoming set of any children
        addIncoming parent inMap = addIncomingLinks parent inMap
        incomingMap' = foldr addIncoming (incomingMap atomSpace') $ preorder identity
    in
        atomSpace' { incomingMap = incomingMap' }
    

containsAtom :: AtomSpace -> AtomIdentity -> Bool
containsAtom atomSpace identity = identity `Set.member` (atomIdentities atomSpace)

--insertList :: Ord a => Set a -> [a] -> Set a
--insertList set list = foldr Set.insert set list

--modify incSet = Set.insert parent incomingForChild
--
--modifyIncoming modify incMap [] = incMap
--modifyIncoming modify incMap ((child, parent):xs) = let
--    plusOne = Map.update modify child incMap
--    in
--        modifyIncoming plusOne xs


removeAtom :: AtomSpace -> AtomIdentity -> AtomSpace
removeAtom atomSpace identity =
    atomSpace {
        atomIdentities = Set.delete identity $ atomIdentities atomSpace,
--        incomingMap = removeIncoming (incoming atomSpace)
        atomStats = Map.delete identity (atomStats atomSpace)
    }

preorder :: AtomIdentity -> [AtomIdentity]
preorder parent = parent : concat [preorder child | child <- out parent]

postorder parent = reverse $ preorder parent

--recursiveOutMap :: AtomIdentity -> [(AtomIdentity, AtomIdentity)]
--recursiveOutMap parent = [(child, parent) | child <- out parent] ++ concat [recursiveOutMap child | child <- out parent]


debug :: Show a => a -> a
debug a = Trace.traceShow a a

setAtomStats :: AtomSpace -> AtomIdentity -> (AtomStats -> Maybe AtomStats) -> AtomSpace
setAtomStats atomSpace id modify =
        atomSpace { atomStats = statsMap' }
    where statsMap  = atomStats atomSpace
          statsMap' = Map.update modify id statsMap

getAtomStats :: AtomSpace -> AtomIdentity -> AtomStats
getAtomStats atomspace id = fromJust $ Map.lookup id $ atomStats atomspace

getSize atomSpace = Set.size $ atomIdentities atomSpace

getTV :: AtomSpace -> AtomIdentity -> TruthValue
getTV atomSpace id = tv $ getAtomStats atomSpace id
setTV :: AtomSpace -> AtomIdentity -> TruthValue -> AtomSpace
setTV atomSpace id truthValue = setAtomStats atomSpace id (\stats -> Just stats { tv = truthValue} )

getAV :: AtomSpace -> AtomIdentity -> AttentionValue
getAV atomSpace id = av $ getAtomStats atomSpace id
setAV :: AtomSpace -> AtomIdentity -> AttentionValue -> AtomSpace
setAV atomSpace id attentionValue = setAtomStats atomSpace id (\stats -> Just stats { av = attentionValue} )

conceptNode = 3
listLink = 42

hacks = do
    let atomspace = emptyAtomSpace
    let node = (AtomIdentity {typeCode = 3, name = "cat", out = []})
    let link = (AtomIdentity {typeCode = 27, name = "", out = [node]})
    putStrLn $ show $ preorder link

    let atomspace2 = addAtom atomspace node
    putStrLn $ show $ atomspace2

    --putStrLn $ show $ recursiveOutMap link
    let atomspace3 = addAtom atomspace2 link
    putStrLn $ show $ atomspace3

    let atomStats = AtomStats { av = NoAV, tv = NoTV }
    let atomspace4 = setAtomStats atomspace3 node (\_ -> Just atomStats)
    putStrLn $ show $ atomspace4

main = do
    args <- getArgs
    let n_nodes = read (head args)
    speedTest n_nodes

speedTest num_nodes = do
    let atomSpace = emptyAtomSpace
    let node_names = Prelude.map show [0..num_nodes]
    let nodes = [node conceptNode name | name <- node_names]
    let links = [link listLink [n1, n2] |(n1,n2) <- zip nodes $ tail nodes]

    --let atomSpace2 = foldr (\atom old_as -> addAtom old_as atom) atomSpace atoms
    let atomSpace2 = List.foldl (\old_as atom -> addAtom old_as atom) atomSpace nodes
    --putStrLn $ show $ getSize atomSpace2

    --let atomSpace2 = foldr (\atom old_as -> addAtom old_as atom) atomSpace atoms
    let atomSpace3 = List.foldl (\old_as atom -> addAtom old_as atom) atomSpace2 links
    --putStrLn $ show $ getSize atomSpace3
    
    let bagOfTVs = List.map (getTV atomSpace3) (nodes++links)
    --let bagOfTVs = List.map (getTV atomSpace3) [nodes !! 13, nodes !! 42]
    putStrLn $ show $ length $ show $ bagOfTVs

    -- force it to evaluate more of the AS!
    --putStrLn $ show $ length $ Set.toList $ atomIdentities atomSpace3
    --putStrLn $ show $ length $ show atomSpace3

    return ()

