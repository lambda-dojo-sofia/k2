type btree = Nil | Node of (float * float) * btree * btree

let tree1 = Node((4.0, 5.0), Nil, Nil)

let isOrderedTree = Node((3.0, 10.0),
                         Node((5.0, 8.0), Node((6.0, 7.0), Nil, Nil), Node((4.0, 9.0), Nil, Nil)),
                         Node((2.0, 12.0), Nil, Node((1.0, 5.0), Nil, Nil)))

let orderedTree tree = match tree with
  | Nil -> true
  | Node(_, Nil, Nil) -> true
  | Node((iStart, iEnd), nil, right) ->
  | Node((iStart, iEnd), left, right) ->
