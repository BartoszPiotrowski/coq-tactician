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

(*
    let geom_mean x y =
        sqrt (x *. y)
*)

    let interleave l1 l2 =
        let rec loop l l1 l2 =
            match l1, l2 with
            | [], l2 -> (List.rev l) @ l2
            | l1, [] -> (List.rev l) @ l1
            | h1 :: t1, h2 :: t2 -> loop (h2 :: h1 :: l) t1 t2
        in
        loop [] l1 l2

    let dedup l =
        let rec loop dl l =
            match l with
            | [] -> List.rev dl
            | (sc, tc) :: t ->
                if List.exists (fun (_, tc') -> tc = tc') dl
                then loop dl t else loop ((sc, tc) :: dl) t
        in
        loop [] l

    let merge_preds preds_knn preds_forest =
        let preds = interleave preds_forest preds_knn in
        let n = List.length preds in
        List.mapi (fun i (s, t) -> (float_of_int (n-i), t)) preds |> dedup

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
