type builder =
  | Builder : (module Obuilder.BUILDER with type t = 'a) * 'a -> builder

type t = {
  pool : unit Current.Pool.t option;
  timeout : Duration.t option;
  level : Current.Level.t option;
  builder : builder;
}

module Key = struct
  type t = {
    spec : [ `File of Fpath.t | `Spec of Obuilder_spec.t ];
    source : [ `No_context | `Git of Current_git.Commit.t ];
  }

  let source_to_json = function
    | `No_context -> `Null
    | `Git commit ->
      `Assoc [ ("git", `String (Current_git.Commit.marshal commit)) ]

  let source_of_json = function
    | `Null -> `No_context
    | `Assoc [ ("git", `String git) ] ->
      `Git (Current_git.Commit.unmarshal git)
    | _ -> failwith "Unknown context for obuilder"

  let spec_to_json = function
    | `File f -> `Assoc [ "path", `String (Fpath.to_string f) ]
    | `Spec spec -> `Assoc [ "spec", `String (Obuilder_spec.sexp_of_t spec |> Sexplib0.Sexp.to_string) ]

  let spec_of_json = function
    | `Assoc [ "spec", `String spec ] ->
      `Spec (Obuilder_spec.t_of_sexp (Sexplib.Sexp.of_string spec))
    | `Assoc [ "path", `String path ] -> `File (Fpath.v path)
    | _ -> failwith "Failed to parse spec!"

  let to_json t =
    `Assoc
      [
        ( "spec", spec_to_json t.spec);
        ("source", source_to_json t.source);
      ]

  let of_json v =
    match Yojson.Safe.from_string v with
    | `Assoc [ ("spec", spec); ("source", v) ] ->
      let spec = spec_of_json spec in
      let source = source_of_json v in
      { spec; source }
    | _ -> failwith "Failed to unmarshal "

  let digest t = Yojson.Safe.to_string (to_json t)
  let pp f t = Yojson.Safe.pretty_print f (to_json t)
end

module Value = Current.String

let id = "obuilder-build"

let (>>!=) f v = Lwt_result.bind f v

let with_context ~job context fn =
  let open Lwt_result.Infix in
  match context with
  | `No_context -> Current.Process.with_tmpdir ~prefix:"build-context-" fn
  | `Dir path ->
    Current.Process.with_tmpdir ~prefix:"build-context-" @@ fun dir ->
    Current.Process.exec ~cwd:dir ~cancellable:true ~job
      ("", [| "rsync"; "-aHq"; Fpath.to_string path ^ "/"; "." |])
    >>= fun () -> fn dir
  | `Git commit -> Current_git.with_checkout ~job commit fn

let job_logger job tag msg =
  match tag with
  | `Heading ->
    Current.Job.log job "%a@." Fmt.(styled (`Fg (`Hi `Blue)) string) msg
  | `Note ->
    Current.Job.log job "%a@." Fmt.(styled (`Fg `Yellow) string) msg
  | `Output -> Current.Job.log job "%s@." msg

let build { builder; timeout; pool; level } (job : Current.Job.t)
    (k : Key.t) : Value.t Current.or_error Lwt.t =
  let open Lwt.Infix in
  let (Builder ((module B), builder)) = builder in
  let level = Option.value level ~default:Current.Level.Average in
  Current.Job.start ?timeout ?pool job ~level >>= fun () ->
  with_context ~job k.source @@ fun dir ->
  let ctx =
    Obuilder.Context.v ~src_dir:(Fpath.to_string dir) ~log:(job_logger job)
      ()
  in
  let spec = match k.spec with
    | `Spec spec -> Lwt_result.return spec
    | `File path -> (
        match Bos.OS.File.read path with
        | Ok s -> Lwt_result.return (Sexplib.Sexp.of_string s |> Obuilder_spec.t_of_sexp)
        | Error e -> Lwt_result.fail e
      )
  in
  spec >>!= fun spec ->
  B.build builder ctx spec >|= function
  | Error `Cancelled -> Error (`Msg "Cancelled")
  | Error (`Msg _) as e -> e
  | Ok _ as v -> v

let pp = Key.pp
let auto_cancel = true