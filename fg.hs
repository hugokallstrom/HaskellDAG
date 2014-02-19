import Graph2

f :: Integer -> Int
f weight = fromIntegral weight

g :: Integer -> Int
g weight = fromIntegral weight

test :: (Num w) => Graph w -> Graph w 
test (Graph v w) = 
	let a = add_vertex (Graph v w) 2
            b = add_vertex a 1
	    c = add_vertex b 2
	    d = add_vertex c 3
	    e = add_edge d 'a' 'b' 3
	    f = add_edge e 'b' 'c' 2
	    g = add_edge f 'a' 'd' 1
	in  g
