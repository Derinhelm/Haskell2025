{-# LANGUAGE RecordWildCards #-}
module FractalTree
    (
        FractalTreeConfig,
        SakuraLeafConfig, 
        defaultTree,
        defaultTreeRoot,
        defaultSakuraLeaf,
        FractalTree(..),
        computeTree,
        SakuraLeaf(..),
        sakuraLeaf,
        branchToCoords,
        branchLength,
        pointToCoords
    ) where

--------------------------------------------------------------------------
----------------------------- Data types ---------------------------------
--------------------------------------------------------------------------
-- x, y coordinates
newtype Point = Point (Float, Float) deriving (Show)

-- Two points indicating start and end points of tree branch
data Branch = Branch {
    pointFrom :: Point,
    pointTo   :: Point,
    len   :: Float
    } deriving (Show)

-- Simple tree consists of list of Branches, collected in right-to-left order
-- FromRoot tree is tree that "grows" from root    
data FractalTree = Simple [Branch] | FromRoot [[Branch]] deriving (Show)

-- Parameter to set tree type
data Mode = SimpleTree | FromRootTree deriving (Show)

-- General parameters for tree computation
data FractalTreeConfig = FractalTreeConfig {
    start            :: Point,
    trunckLength     :: Float,
    minBranchLength  :: Float,
    lengthDecrease   :: Float,
    rightBranchAngle :: Float,
    leftBranchAngle  :: Float,
    mode             :: Mode
    } deriving (Show)

-- Current data for next branch computation
data CurTreeIter = CurTreeIter {
    coord_x  :: Float,
    coord_y  :: Float,
    cur_a    :: Float,
    cur_b    :: Float
}

data SakuraLeaf = SakuraLeaf [Point] deriving (Show)
data SakuraLeafConfig = SakuraLeafConfig {
    sideLength :: Float,
    insideLength :: Float,
    fullLeafAngle :: Float
} deriving (Show)
--------------------------------------------------------------------------
----------------------------- Constants ----------------------------------
--------------------------------------------------------------------------
defaultTree :: FractalTreeConfig
defaultTree = FractalTreeConfig {
    start            = Point (0, 0),
    trunckLength     = 200,
    minBranchLength  = 4,
    lengthDecrease   = 0.7,
    rightBranchAngle = pi / 6,
    leftBranchAngle  = pi / 4,
    mode             = SimpleTree
}

defaultTreeRoot :: FractalTreeConfig
defaultTreeRoot = FractalTreeConfig {
    start            = Point (0, 0),
    trunckLength     = 250,
    minBranchLength  = 4,
    lengthDecrease   = 0.7,
    rightBranchAngle = pi / 6,
    leftBranchAngle  = pi / 4,
    mode             = FromRootTree
}

defaultSakuraLeaf :: SakuraLeafConfig
defaultSakuraLeaf = SakuraLeafConfig{
    sideLength    = 5,
    insideLength  = 7,
    fullLeafAngle = 72*pi/180
}
--------------------------------------------------------------------------
------------------------------ Functions ---------------------------------
--------------------------------------------------------------------------
computeTree :: FractalTreeConfig -> FractalTree
computeTree config  = case mode config of
                        SimpleTree ->  Simple (computeBranches config (CurTreeIter {coord_x=x, coord_y=y, cur_a=(trunckLength config), cur_b=(-pi/2)}))
                                            where 
                                                x = getX (start config)
                                                y = getY (start config)
                        FromRootTree -> FromRoot $ toBranches (computeBranchesRoot config [])

computeBranches :: FractalTreeConfig -> CurTreeIter -> [Branch]
computeBranches config (CurTreeIter x y a b) 
    | a > (minBranchLength config)= [branch] ++ left_subtree ++ rigth_subtree
    | otherwise = []
        where 
            new_a = a * (lengthDecrease config)
            new_x =  x + new_a * cos b
            new_y =  y - new_a * sin b
            branch = Branch (Point (x, y)) (Point (new_x, new_y)) new_a
            left_subtree = computeBranches  config (CurTreeIter new_x new_y new_a (b + (leftBranchAngle config)))
            rigth_subtree = computeBranches  config (CurTreeIter new_x new_y new_a (b - (rightBranchAngle config))) 

computeBranchesRoot :: FractalTreeConfig -> [(Branch, Float, Float)] -> [[(Branch, Float, Float)]]
computeBranchesRoot config [] = [[br]] ++ computeBranchesRoot config [br]
                                where 
                                    new_a = (trunckLength config) * (lengthDecrease config)
                                    b = (-pi/2)
                                    new_x = (getX (start config)) + new_a * cos b
                                    new_y = (getY (start config)) - new_a * sin b
                                    br = (Branch (start config) (Point (new_x, new_y)) new_a, new_a, b)
computeBranchesRoot config ((Branch p1 p2 l, a, b):branches) 
    | a > (minBranchLength config) = [layer] ++ computeBranchesRoot config layer
    | otherwise = []
    where 
        layer = computeLayer config ((Branch p1 p2 l, a, b):branches) 


computeLayer :: FractalTreeConfig -> [(Branch, Float, Float)] -> [(Branch, Float, Float)]
computeLayer config prevLayer = concat $ map (helpFunc config) prevLayer

helpFunc :: FractalTreeConfig -> (Branch, Float, Float) -> [(Branch, Float, Float)]
helpFunc config (Branch _ p2 _, a, b) = [br1, br2]
        where 
            new_a = a * (lengthDecrease config)
            b1 = b + (leftBranchAngle config)
            b2 = b - (rightBranchAngle config)
            new_x1 = (getX p2) + new_a * cos b1
            new_y1 = (getY p2) - new_a * sin b1
            new_x2 = (getX p2) + new_a * cos b2
            new_y2 = (getY p2) - new_a * sin b2
            br1 = (Branch p2 (Point (new_x1, new_y1)) new_a, new_a, b1)
            br2 = (Branch p2 (Point (new_x2, new_y2)) new_a, new_a, b2)

toBranches :: [[(Branch, Float, Float)]] -> [[Branch]]
toBranches [] = []
toBranches (brs:branches) = (flatBranches brs) : toBranches branches

flatBranches :: [(Branch, Float, Float)] -> [Branch]
flatBranches [] = []
flatBranches ((br, _, _):branches) = br : (flatBranches branches)

getX :: Point -> Float
getX (Point (x, _)) = x

getY :: Point -> Float
getY (Point (_, y)) = y

pointToCoords :: Point -> (Float, Float)
pointToCoords (Point xy) = xy

branchToCoords :: Branch -> [(Float, Float)]
branchToCoords (Branch startPoint endPoint _) = [(getX startPoint, getY startPoint), (getX endPoint, getY endPoint)]

branchLength :: Branch -> Float
branchLength (Branch _ _ l) = l

sakuraLeaf ::  SakuraLeafConfig -> (Float, Float) -> SakuraLeaf
sakuraLeaf config  (x, y) = SakuraLeaf [
    (Point (x, y)),
    (Point (x, y + l1)),
    (Point (x + l2 * cos (pi/2 - b), y + l2 * sin (pi/2 - b))),
    (Point (x + l1 * cos (pi/2 - 2*b), y + l1 * sin (pi/2 - 2*b))),
    (Point (x + l2 * cos (pi/2 - 3*b), y + l2 * sin (pi/2 - 3*b))),
    (Point (x + l1 * cos (pi/2 - 4*b), y + l1 * sin (pi/2 - 4*b))),
    (Point (x, y))
    ]
    where
        b = ((fullLeafAngle config) / 4)
        l1 = (sideLength config) 
        l2 = (insideLength config) 
