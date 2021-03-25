module type DATA = sig
    type feature
    type features
    type 'a example = features * ('a option)
    type 'a examples = 'a example list
    type direction = Left | Right
    type rule = features -> direction
    val rule_of_fea : feature -> rule
    val is_empty : 'a examples -> bool
    val add : 'a examples -> 'a example -> 'a examples
    val features : 'a example -> features
    val split : feature -> 'a examples -> 'a examples * 'a examples
    val gini_rule : ?m:int -> 'a examples -> feature
    val random_label : 'a examples -> 'a
    val random_example : 'a examples -> 'a example
    val fold_left : ('a -> 'b example -> 'a) -> 'a -> 'b examples -> 'a
    val label : 'a example -> 'a
    val labels : 'a examples -> 'a list
end

module Make = functor (Data : DATA) -> struct

    type 'a tree =
        | Node of Data.feature * ('a tree) * ('a tree)
        | Leaf of 'a * ('a Data.examples)

    let leaf example =
        Leaf (Data.label example, [example])

    (* returns Node(split_rule, Leaf (label1, stats1), Leaf(label2, stats2)) *)
    let make_new_node examples =
        let fea = Data.gini_rule examples in
        let examples_l, examples_r = Data.split fea examples in
        if Data.is_empty examples_l || Data.is_empty examples_r
        then Leaf(Data.random_label examples, examples)
        else Node(fea,
            Leaf(Data.random_label examples_l, examples_l),
            Leaf(Data.random_label examples_r, examples_r))

    let extend examples =
        let labels = Data.labels examples in
        let imp = Impurity.gini_impur labels in
        imp > 0.5
    (* TODO more sophisticated condition needed *)

    (* pass the example to a leaf; if a condition is satisfied, extend the tree *)
    let add (tree : 'a tree) (example : 'a Data.example) : 'a tree =
        let rec loop = function
            | Node (fea, tree_l, tree_r) ->
                (match (Data.rule_of_fea fea) (Data.features example) with
                | Left  -> Node(fea, loop tree_l, tree_r)
                | Right -> Node(fea, tree_l, loop tree_r))
            | Leaf (label, examples) ->
                let examples = Data.add examples example in
                if extend examples then make_new_node examples
                else Leaf (label, examples)
        in
        loop tree

    let tree examples =
        let example = Data.random_example examples in
        Data.fold_left add (leaf example) examples

    let classify example tree =
        let rec loop tree =
            match tree with
            | Leaf (cls, _) -> cls
            | Node (fea, tree_l, tree_r) ->
                (match (Data.rule_of_fea fea) (Data.features example) with
                | Left  -> loop tree_l
                | Right -> loop tree_r)
        in loop tree

end
