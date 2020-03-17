module Quadtree where

import Data.Array
import Data.Word

import QuadtreeDef

-- given quadtree, get the associations (the coordinates and grayscale value)
getAssocs :: Quadtree                -- the quadtree
          -> [((Int, Int), Word8)]  -- the assocs of the quadtree
-- if the quadtree has no children, 
-- the values of all coordinates is the average grayscale value
getAssocs (QNode x y width avg Q0) = assocs
  where 
    assocs = [((i, j), avg)
             | i <- [x ..(x + width - 1)], j <- [y..y + width - 1]]
-- if the quadtree has 4 children, 
-- the associations are the associations of the children concatenated
getAssocs (QNode x y width avg (Q4 kid1 kid2 kid3 kid4)) = assocs
  where 
    assocs = getAssocs kid1 ++ getAssocs kid2
             ++ getAssocs kid3 ++ getAssocs kid4

-- given a quadtree, get the picture
quadtreeToPic :: Quadtree                -- the quadtree
              -> Array (Int, Int) Word8  -- the picture
quadtreeToPic (QNode x y width avg kids) = pic
  where
    pic = array ((x, y), (x + width - 1, y + width - 1))  -- array with the
          (getAssocs (QNode x y width avg kids))          -- quadtree's assocs

-- given a picture, get the maximum grayscale difference
getMaxDiff :: Array (Int, Int) Word8  -- the picture
           -> Word8                   -- maximum grayscale difference
getMaxDiff pic = maxDiff 
  where 
    maxDiff = max - min        -- difference of max and min grayscale values
    max = maximum (elems pic)  -- the maximum grayscale value
    min = minimum (elems pic)  -- the minimum grayscale value

-- given a picture, get the rounded average grayscale value
getAvg :: Array (Int, Int) Word8  -- the picture
          -> Word8                -- the rounded average grayscale value
getAvg pic = roundedAvg
  where 
    -- round the average
    roundedAvg = round unroundedAvg       -- the rounded avg grayscale value
    -- get the average by dividing total by number of grayscale values
    unroundedAvg = totalFrac / len        -- the unrounded avg grayscale value
    -- convert the total to fractional
    totalFrac = realToFrac (total)        -- sum of grayscale values (frac)
    total = sum (map toInteger elements)  -- sum of grayscale values (Integer)
    len = fromIntegral (length elements)  -- the number of grayscale values
    elements = elems pic                  -- the grayscale values of the pic

-- given threshold, depth cap, picture, bounds, width,
-- gets the child quadtrees of the picture (quadtrees of the top left square,
-- the botton left square, the top right square, the bottom right square)
subdivide :: Word8                     -- threshold
          -> Int                       -- depth cap
          -> Array (Int, Int) Word8    -- picture
          -> ((Int, Int), (Int, Int))  -- bounds
          -> Int                       -- width
          -> QKids                     -- the children quadtrees
subdivide threshold depthCap pic ((x1, y1), (x2, y2)) width = k
  where
    k = (Q4 kid1 kid2 kid3 kid4)
    -- get the quadtrees of each square (the top left square,
    -- the botton left square, the top right square, the bottom right square)
    kid1 = picToQuadtree threshold depthCap pic1  -- top left quadtree
    kid2 = picToQuadtree threshold depthCap pic2  -- bottom left quadtree
    kid3 = picToQuadtree threshold depthCap pic3  -- top right quadtree
    kid4 = picToQuadtree threshold depthCap pic4  -- bottom right quadtree
    -- get the top left square, bottom left square, top right square,
    -- bottom right square as separate pictures
    pic1 = ixmap ((x1, y1), (x1 + width - 1, y1 + width - 1)) -- top left pic
           (\i -> i) pic
    pic2 = ixmap ((x1, y1 + width), (x1 + width - 1, y2))     -- bottom 
           (\i -> i) pic                                      -- left pic
    pic3 = ixmap ((x1 + width, y1), (x2, y1 + width - 1))     -- top right pic
           (\i -> i) pic
    pic4 = ixmap ((x1 + width, y1 + width), (x2, y2))         -- bottom
           (\i -> i) pic                                      -- right pic

-- given threshold, deapth cap, picture, convert it to quadtree
picToQuadtree :: Word8                   -- threshold
              -> Int                     -- depth cap
              -> Array (Int, Int) Word8  -- pic
              -> Quadtree
picToQuadtree threshold depthCap pic = quadtree
  where
    ((x1, y1), (x2, y2)) = bounds pic        -- the bounds
    width = x2 - x1 + 1                      -- the width
    quadtree = (QNode x1 y1 width avg kids)  -- the quadtree
    avg = getAvg pic                         -- the average
    kids                                     -- the children
        -- if depthCap > 0 and maximum difference > threshold
        -- then subdivide the pic (quadtree have 4 children)
        | (depthCap > 0 && (getMaxDiff pic) > threshold)
            = subdivide threshold (depthCap - 1) 
            pic ((x1, y1), (x2, y2)) (width `div` 2)
        -- otherwise, don't subdivide (quadtree have no children)
        | otherwise = Q0
