(** {1 OCurrent Plugin for OBuilder}

    This library provides an OCurrent interface to OBuilder.
    {{: https://github.com/ocurrent/obuilder} OBuilder} is a [docker build]-like library that provides
    a way to run build jobs in a sandboxed environment.
*)

(** {1 Core Library} *)

type builder =
  | Builder : (module Obuilder.BUILDER with type t = 'a) * 'a -> builder
  (** A builder is a way to hide the type in an existential so you can
      build your implementation (choosing the type of store for example) 
      outside of the OCurrent functions and pass it in. *)

val build : 
  ?level:Current.Level.t ->
  ?timeout:Duration.t ->
  ?schedule:Current_cache.Schedule.t ->
  ?label:string ->
  ?pool:unit Current.Pool.t ->
  builder ->
  [ `Spec of Obuilder_spec.t | `File of Fpath.t ] Current.t ->
  [< `Git of Current_git.Commit.t Current.t | `No_context ] ->
  Build.Value.t Current.t
(** [build builder spec src] will use OBuilder to build the [spec] that you provide 
    in the context of [src]. Typically in your OCurrent pipeline you should only use
    a single [builder] instance. 

    @param level This is the {!Current.Level.t} for the job, defaulting to {! Current.Level.Average}.
    @param timeout How long a build is allowed to run before being cancelled.
    @param schedule Can be used to rebuild periodically.
    @param label Is primarily used for the OCurrent dot graph.
    @param pool Can be used to limit the number of concurrent builds happening.
*)

(** {1 Raw OCurrent Primitives}

    You probably don't need these, but they're exposed in case you do. *)

module Raw : sig
  val build :
    ?pool:unit Current.Pool.t ->
    ?timeout:Duration.t ->
    ?level:Current.Level.t ->
    ?schedule:Current_cache.Schedule.t ->
    builder ->
    [ `File of Fpath.t | `Spec of Obuilder_spec.t ] ->
    [ `Git of Current_git.Commit.t | `No_context ] ->
    string Current.Primitive.t 
end