module Git = Current_git
module Build = Current_obuilder

let timeout = Duration.of_hour 1
let schedule = Current_cache.Schedule.v ~valid_for:(Duration.of_day 7) ()

let pipeline ~builder () =
  let src = Git.clone ~schedule "https://github.com/patricoferris/current-obuilder" in
  let spec = Current.return (`File (Fpath.v "obuilder.spec")) in
  Build.build ~timeout builder spec (`Git src) |>
  Current.ignore_value

let main config store sandbox =
  let builder =
    let open Lwt.Infix in
    store
    >>= fun (Obuilder.Store_spec.Store ((module Store), store)) ->
    Obuilder.Sandbox.create ~state_dir:"obuilder-state" sandbox
    >>= fun sandbox ->
    let module Builder =
      Obuilder.Builder (Store) (Obuilder.Sandbox) (Obuilder.Docker)
    in
    Lwt.return
    @@ Current_obuilder.Builder
      ((module Builder), Builder.v ~store ~sandbox)
  in
  let builder = Lwt_main.run builder in
  Lwt_main.run begin
    let engine = Current.Engine.create ~config (pipeline ~builder) in
    Lwt.choose [
      Current.Engine.thread engine;
    ]
  end

(* Command-line parsing *)

open Cmdliner

let cmd =
  let doc = "Build the current-builder repository." in
  let info = Cmd.info "current-obuilder-builder" ~doc in
  Cmd.v info Term.(term_result (const main $ Current.Config.cmdliner $ Obuilder.Store_spec.cmdliner $ Obuilder.Sandbox.cmdliner))

let () = exit @@ Cmd.eval cmd