module Make = functor (Data : Tree_online.DATA) -> struct
    module Tree = Tree_online.Make(Data)

    let empty = []

    let add ?(min_impur=0.5) ?(n_trees=100) ?(remove_old=false)
        forest example =
        let n = List.length forest in
        let add_tree = (n = 0) || ((Random.int n = 0) && n < n_trees) in
        let del_tree = remove_old && n >= n_trees in
        let forest = if del_tree then Utils.remove_last forest else forest in
        let updated_trees =
            List.map (fun tree -> Tree.add ~min_impur tree example) forest in
        if add_tree then Tree.leaf example :: updated_trees else updated_trees

    let forest examples =
        Data.fold_left add empty examples

    let vote votes =
        let freqs = Utils.freqs votes in
        List.sort (fun (_, c1) (_, c2) -> compare c2 c1) freqs

    let classify forest example =
        let votes = List.map (Tree.classify example) forest in
        match vote votes with
        | (e, _) :: _ -> e
        | [] -> failwith "empty list of voting scores"

    let score forest unlabeled_example =
        let votes = List.map (Tree.classify unlabeled_example) forest in
        List.map (fun (a, b) -> (b, a) ) (vote votes)

end
