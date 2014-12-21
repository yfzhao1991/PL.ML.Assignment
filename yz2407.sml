Control.Print.printDepth := 100;
Control.Print.printLength := 100;

(* 1. *)
fun intPartitionSort [] = []
|   intPartitionSort (x::xs) = 
      let fun Partition p ([], front, back) =  (intPartitionSort front) @ [p] @ (intPartitionSort back)
          |   Partition p (x::xs, front, back) = 
                if x < p then 
                   Partition p (xs, x::front, back)
                else 
                   Partition p (xs, front, x::back)
      in
         Partition x (xs, [], [])
      end

(* 2. *)
fun partitionSort f [] = []
|   partitionSort f (x::xs) = 
	  let fun Partition p f ([], front, back) =  (partitionSort f front) @ [p] @ (partitionSort f back)
		  |   Partition p f (x::xs, front, back) = 
				if f(x,p) then 
                   Partition p f (xs, x::front, back)
                else 
                   Partition p f (xs, front, x::back)
	  in
         Partition x f (xs, [], [])
      end

(* 3. *)
datatype 'a tree = leaf of 'a | node of 'a tree list

(* 4. *)
fun sortTree f (leaf x)=leaf(partitionSort f x)
| sortTree f (node [])=node []
| sortTree f (node n)=
	let
		val t1=sortTree f (hd n)
		val node t2=sortTree f (node (tl n))
	in node(t1::t2)
	end

(* 5. *)
fun merge f [] [] = []
|   merge f [] x = x
|   merge f x [] = x
|	merge f (x::xs) (y::ys)=
	if f(x,y) then
		x::(merge f xs (y::ys))
	else
		y::(merge f (x::xs) ys)
		
(* 6. *)
fun mergeTree f (leaf x)=partitionSort f x
| mergeTree f (node [])=[]
| mergeTree f (node n)=
	let
		val t1=mergeTree f (hd n)
		val t2=mergeTree f (node (tl n))
	in merge f t1 t2
	end
