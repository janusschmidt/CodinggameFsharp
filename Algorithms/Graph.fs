module Algorithms.Graph

type nodeId<'a when 'a : equality> = 'a
type edge<'a when 'a : equality> = {node1:nodeId<'a>; node2:nodeId<'a>; weight:int}
type node<'a when 'a : equality> = {id:nodeId<'a>; shortestDistance:int; path:nodeId<'a> list}
type graph<'a when 'a : equality> = {nodes:node<'a> list; focusNode:nodeId<'a>; visitedNodes:nodeId<'a> list}
