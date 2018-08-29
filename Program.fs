open System
open FSharpPlus
open FSharpPlus.Data

let run = ReaderT.run

//all generic types go here 
module Types = 
    //all the domain types go here
    module Domain = 
        type User = 
            {UserId : string;
             UserEmail : string;
             UserPassword : string}
    
    type Err = Err of string 
    and Conf = {conn: string} 

    and Env = {userRepo : UserRepo; version : string; conf: Conf }

    and App<'a> = ReaderT<Env, Result<'a, Err>>
    and  UserRepo = 
        {getUserById : string -> App<Domain.User option>;
         createUser : Domain.User -> App<unit>}
    
open Types
open Types.Domain


type AppMonadBuilder ()  =
    let x = new MonadFxBuilder()
    member __.Delay (expr: _->App<'T>) = x.Delay(expr)
    member __.Run f = x.Run(f)

    member  __.ReturnFrom (expr): App<'T> = expr                                       
    member __.TryWith    (expr, handler     ) = x.TryWith(expr, handler)
    member __.TryFinally (expr, compensation) = x.TryFinally(expr, compensation)
    member __.Using (disposable, body) = x.Using(disposable, body)
    member  inline __.Return (v: 'T) : App<'T> = result v
    member  inline __.Yield (v: 'T) : App<'T> = result v
    member  __.Combine (a: App<'T>, b) = x.Combine(a, b) : App<'T>
    member  __.Zero (): App<unit> = result ()
    member  __.While (guard, body): App<unit> = x.While(guard, body)
    member  __.For (p, rest) : App<unit> = x.For(p, rest)
    member inline __.Bind (p: App<'T>, rest: 'T->App<'U>) : App<'U> = p >>= fun x -> 
        try rest x with e -> lift <| Error (Err e.Message) 

    //  [<CustomOperation("bind", MaintainsVariableSpaceUsingBind = true)>]
    member inline __.BindEx (p: App<'T>,  [<ProjectionParameter>] rest: 'T->App<'U>) : App<'U> = p >>= fun x -> 
        try rest x with e -> lift <| Error (Err e.Message) 


let flow  = new AppMonadBuilder()

 //let flow  = new Builders.MonadFxBuilder()

//big module with SQL implementations and mappersTypes.
[<RequireQualifiedAccess>]
module PgSql = 
    open Npgsql.FSharp
    
    let conn = 
        Sql.host "localhost"
        |> Sql.port 5432
        |> Sql.username "postgres"
        |> Sql.password "password"
        |> Sql.database "inlinedb"
        |> Sql.str
        |> Sql.connect
    
    let userMapper = 
        function 
        | ["user_id", String userId; 
           "user_email", String userEmail; 
           "user_password", String userPassword] -> 
            Some {UserId = userId;
                  UserEmail = userEmail;
                  UserPassword = userPassword}
        | _ -> None
    
    let getUserById id : App<User option> = 
        flow {
            return conn
                |> Sql.query 
                       "select user_id,user_email,user_password from users where user_id = @userId"
                |> Sql.parameters ["userId", String id]
                |> Sql.executeTable
                |> Sql.mapEachRow userMapper
                |> List.tryHead
        }
    
    let createUser user : App<unit>= 
        flow {
            return conn
            |> Sql.query 
                   "insert into users(user_id,user_email,user_password) values(@userId, @userEmail, @userPassword)"
            |> Sql.parameters ["userId", String user.UserId;
                               "userEmail", String user.UserEmail;
                               "userPassword", String user.UserPassword]
            |> Sql.executeNonQuery
            |> ignore
        }

//our main program
module Main = 
    let getUser (userId: string) : App<User option> =  
        flow {
            let! env = ask 
            let! user = env.userRepo.getUserById userId

            //do! lift (Error (Err "test-err"))
            return user
        }

    let insertNewUser : App<string> = 
        flow {
            let! env = ask
            let id = Guid.NewGuid().ToString()
            let newUser = 
                {UserId = id;
                 UserEmail = sprintf "%s@example.com" id;
                 UserPassword = "12345"}
            do! env.userRepo.createUser newUser
            return id
        } 
    let run : App<User option> = 
        flow {
            let! id = insertNewUser
            return! getUser id
        }

//all bindings between contracts and implementations go here
module Injector = 
    let version = "0.1"
    let impl = 
        {userRepo = 
            {getUserById = PgSql.getUserById; 
             createUser = PgSql.createUser ;} 
         version = version;
         conf = {conn = ""}}


[<EntryPoint>]
let main argv =
    let res = ReaderT.run Main.run Injector.impl
    match res with 
        | Ok v -> printfn "%A" v
        | Error (Types.Err msg) -> printfn "%s - %s"  msg "nice formatted"
        
    0 // return an integer exit code
