open Tactic_learner
open Learner_helper

module OnlineForestPlusKnn : TacticianOnlineLearnerType = functor (TS : TacticianStructures) -> struct
    module LH = L(TS)
    open TS
    open LH
    module Tree = Tree_online.Make(Data)
    module Forest = Forest_online.Make(Data)

    type db_entry =
        { features : feature list;
          obj      : tactic
        }
    type knn =
        { entries     : db_entry list
        ; length      : int
        ; frequencies : int Frequencies.t}
    type forest = (TS.tactic Tree.tree) list
    type model = {forest : forest; knn : knn}

(*     let empty () = {forest = []; examples = Data.empty} *)

    let empty_forest = []
    let empty_knn = {entries = []; length = 0; frequencies = Frequencies.empty}
    let empty () = {forest = empty_forest; knn = empty_knn}

    let add_forest forest b obj =
      let feats = proof_state_to_ints b in
      Forest.add forest (Data.labeled (feats, obj))

    let rec deletelast = function
      | [] -> assert false
      | [x] -> (x.features, [])
      | x::ls' -> let (last, lsn) = deletelast ls' in (last, x::lsn)

    let add_knn db b obj =
      let feats = proof_state_to_ints b in
      let comb = {features = feats; obj = obj} in
      let newfreq = List.fold_left
          (fun freq f ->
             Frequencies.update f (fun y -> Some ((default 0 y) + 1)) freq)
          db.frequencies
          feats in
      let max = 1000 in
      let last, purgedentries = if db.length >= max then deletelast db.entries else ([], db.entries) in
      let newfreq = List.fold_left
          (fun freq f ->
             Frequencies.update f (fun y -> Some ((default 1 y) - 1)) freq)
          newfreq
          last in
      (* TODO: Length needs to be adjusted if we want to use multisets  *)
      let l = if db.length >= max then db.length else db.length + 1 in
      {entries = comb::purgedentries; length = l; frequencies = newfreq}

    let add model b obj =
        {
            forest = add_forest model.forest b obj;
            knn = add_knn model.knn b obj
        }

    let learn db _loc outcomes tac =
      List.fold_left (fun db out -> add db out.before tac) db outcomes

    let predict_forest forest f =
      let feats = proof_state_to_ints (List.hd f).state in
      let example = Data.unlabeled feats in
      let out = Forest.score forest example in
      remove_dups_and_sort out

    let predict_knn db f =
      let feats = proof_state_to_ints (List.hd f).state in
      let tdidfs = List.map
          (fun ent -> let x = tfidf db.length db.frequencies feats ent.features in (x, ent.obj))
          db.entries in
      remove_dups_and_sort tdidfs

    let geom_mean x y =
        sqrt (x *. y)

    let merge_preds preds_knn preds_forest =
        let preds_knn_forest =
            List.map
            (fun (c, t) ->
                let (cf, tf) =
                    try List.find (fun (cf, tf) -> t = tf) preds_forest
                    with Not_found -> (c, t) in
                (geom_mean c cf, t))
            preds_knn in
        let preds_forest_knn =
            List.map
            (fun (c, t) ->
                let (cf, tf) =
                    try List.find (fun (cf, tf) -> t = tf) preds_knn
                    with Not_found -> (c, t) in
                (geom_mean c cf, t))
            preds_forest in
        preds_knn_forest @ preds_forest_knn

    let predict db f =
        if f = [] then IStream.empty else
        let preds_knn = predict_knn db.knn f in
        let preds_forest = predict_forest db.forest f in
        let out = merge_preds preds_knn preds_forest in
        let out = List.map
            (fun (a, c) -> { confidence = a; focus = 0; tactic = c }) out in
        IStream.of_list out

    let evaluate db _ _ = 1., db

end

let () = register_online_learner "online-forest-plus-knn"
    (module OnlineForestPlusKnn)
