module Git = Current_git

type builder = Build.builder =
  | Builder : (module Obuilder.BUILDER with type t = 'a) * 'a -> builder

module Raw = struct
  module BuildC = Current_cache.Make (Build)

  let build ?pool ?timeout ?level ?schedule builder spec source =
    let key = Build.Key.{ spec; source } in
    let ctx = Build.{ pool; timeout; level; builder } in
    BuildC.get ?schedule ctx key
end

let pp_sp_label = Fmt.(option (sp ++ string))

let get_build_context = function
  | `No_context -> Current.return `No_context
  | `Git commit -> Current.map (fun x -> `Git x) commit

let build ?level ?timeout ?schedule ?label ?pool builder spec src =
  let open Current.Syntax in
  Current.component "build%a" pp_sp_label label
  |> let> commit = get_build_context src and> spec = spec in
  Raw.build ?timeout ?pool ?level ?schedule builder spec commit

