open OUnit2
open Pcre2

let test_split _ =
  let assert_splits ?(max = -1) ~pat subj expected =
    let actual = split ~max ~pat subj in
    assert_equal (List.length expected) (List.length actual);
    List.iter2 assert_equal expected actual
  in
  (* An empty string should split to nothing. *)
  assert_splits ~pat:"" "" [];
  assert_splits ~pat:"," "" [];
  (* Split a string on a single character, allowing an unlimited number of
     substrings to be returned with no truncation. *)
  assert_splits ~pat:"," "a" ["a"];
  assert_splits ~pat:"," "a," ["a"; ""];
  assert_splits ~pat:"," "a,b" ["a"; "b"];
  assert_splits ~pat:"," "a,,b" ["a"; ""; "b"];
  assert_splits ~pat:"," "a,,b,," ["a"; ""; "b"; ""; ""];
  (* If ~max:0, then trailing empty strings should be stripped. *)
  assert_splits ~max:0 ~pat:"," "" [];
  assert_splits ~max:0 ~pat:"," "," [];
  assert_splits ~max:0 ~pat:"," "a,,b" ["a"; ""; "b"];
  assert_splits ~max:0 ~pat:"," "a,,b,," ["a"; ""; "b"];
  (* If a positive ~max match count is specified, then we should stop splitting
     the string once that count has been reached. Trailing empty strings are not
     stripped in this mode. *)
  assert_splits ~max:1 ~pat:"," "a,b,c" ["a,b,c"];
  assert_splits ~max:2 ~pat:"," "a,b,c" ["a"; "b,c"];
  assert_splits ~max:3 ~pat:"," "a,b,c" ["a"; "b"; "c"];
  assert_splits ~max:4 ~pat:"," "a,b,c" ["a"; "b"; "c"];
  assert_splits ~max:4 ~pat:"," "a,b,c,," ["a"; "b"; "c"; ","];
  assert_splits ~max:5 ~pat:"," "a,b,c,," ["a"; "b"; "c"; ""; ""];
  (* Multi-character splits. *)
  assert_splits ~pat:"<>" "" [];
  assert_splits ~pat:"<>" "a<" ["a<"];
  assert_splits ~pat:"<>" "a<>b<>c" ["a"; "b"; "c"];
  assert_splits ~pat:"<>" "a<><>b<><>c" ["a"; ""; "b"; ""; "c"];
  assert_splits ~max:0 ~pat:"<>" "a<><>b<><>c<><>" ["a"; ""; "b"; ""; "c"];
  (* If capture groups are present in the split pattern, we should include them
     in the output. *)
  assert_splits ~pat:"([*+])" "" [];
  assert_splits ~pat:"([*+])" "*" [""; "*"; ""];
  assert_splits ~pat:"([*+])" "+" [""; "+"; ""];
  assert_splits ~pat:"([*+])" "m*x+b" ["m"; "*"; "x"; "+"; "b"]

let test_full_split _ =
  assert_equal [Text "ab"; Delim "x"; Group (1, "x"); NoGroup; Text "cd"]
      (full_split ~pat:"(x)|(u)" "abxcd")
  ; assert_equal [Text "ab"; Delim "x"; Group (1, "x"); NoGroup; Text "cd"; Delim "u";
                  NoGroup; Group (2, "u"); Text "ef"]
      (full_split ~pat:"(x)|(u)" "abxcduef")

let test_exec_all _ =
  let assert_matches ~pat subj expected =
    let rec assert_match_equal idx expected actual =
      match expected with
      | (exp_subj, exp_start, exp_stop) :: exp_tl ->
          assert_equal exp_subj (get_substring actual idx);
          let act_start, act_stop = get_substring_ofs actual idx in
          assert_equal exp_start act_start;
          assert_equal exp_stop act_stop;
          assert_match_equal (succ idx) exp_tl actual
      | [] -> ()
    and assert_substrings_equal expected actual =
      match expected, actual with
      | exp_hd :: exp_tl, act_hd :: act_tl ->
          assert_equal (List.length exp_hd) (num_of_subs act_hd);
          assert_match_equal 0 exp_hd act_hd;
          assert_substrings_equal exp_tl act_tl
      | [], [] -> ()
      | _, _ -> failwith "Unequal substring list lengths"
    in
    let actual = exec_all ~pat subj in
    assert_equal (List.length expected) (Array.length actual);
    assert_substrings_equal expected @@ Array.to_list actual
  in
  (* A pattern with no matches should return an empty array. *)
  assert_matches ~pat:"empty" "" [];
  assert_matches ~pat:"empty" "empt" [];
  (* Single matches of non-zero-length patterns. *)
  assert_matches ~pat:"p" "p" [[("p", 0, 1)]];
  assert_matches ~pat:"pattern" "pattern" [[("pattern", 0, 7)]];
  (* Multiple matches of non-zero-length patterns. *)
  assert_matches ~pat:"a" "aaa" [[("a", 0, 1)]; [("a", 1, 2)]; [("a", 2, 3)]];
  assert_matches ~pat:"hello|ocaml" "hello ocaml"
    [[("hello", 0, 5)]; [("ocaml", 6, 11)]];
  assert_matches ~pat:"(hello|ocaml)" "hello ocaml"
    [[("hello", 0, 5); ("hello", 0, 5)]; [("ocaml", 6, 11); ("ocaml", 6, 11)]];
  (* Matches of zero-length patterns. *)
  assert_matches ~pat:"(?=(hello|ocaml))" "hello ocaml"
    [[("", 0, 0); ("hello", 0, 5)]; [("", 6, 6); ("ocaml", 6, 11)]];
  assert_matches ~pat:"(?=(hello|ocaml))" "hellocaml"
    [[("", 0, 0); ("hello", 0, 5)]; [("", 4, 4); ("ocaml", 4, 9)]]

let suite = "Test pcre2" >::: [
      "test_split"      >:: test_split;
      "test_full_split" >:: test_full_split;
      "test_exec_all"   >:: test_exec_all;
    ]

let _ = if not !Sys.interactive then run_test_tt_main suite
