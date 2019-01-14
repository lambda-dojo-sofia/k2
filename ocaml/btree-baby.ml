type btree = Nil | Node of (float * float) * btree * btree

let tree1 = Node((4.0, 5.0), Nil, Nil)

let isOrderedTree =
    Node((3.0, 10.0),
         Node((5.0, 8.0), Node((6.0, 7.0), Nil, Nil), Node((4.0, 9.0), Nil, Nil)),
         Node((2.0, 12.0), Nil, Node((1.0, 15.0), Nil, Nil)))

let inInterval a b node = match node with
  | Nil -> true
  | Node((x, y), _, _) -> a >= x && a <= y && b >= a && b <= y

let nodeContains b a = match a with
  | Nil -> true
  | Node((ax, ay), _, _) -> inInterval ax ay b

let rec orderedTree tree = match tree with
  | Nil -> true
  | Node(_, Nil, Nil) -> true
  | Node(_, Nil, right) -> nodeContains right tree && orderedTree right
  | Node(_, left, Nil) -> nodeContains tree left && orderedTree left
  | Node(_, left, right) ->
          nodeContains right tree && nodeContains tree left &&
          orderedTree right && orderedTree left

let () = match (orderedTree isOrderedTree) with
  | true -> print_endline "true"
  | false -> print_endline "false"
