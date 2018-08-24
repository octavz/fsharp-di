open System
open FSharpPlus
open FSharpPlus.Data


//all generic types go here 
module Types = 
    //all the domain types go here
    module Domain = 
        type User = 
            {UserId : string;
             UserEmail : string;
             UserPassword : string}
    
    //repositories contracts go here
    module Repositories = 
        open Domain

        type UserRepo = 
            {getUserById : string -> User option;
             createUser : User -> unit}
    
    //generic types
    type Err = 
        {message : String}
    
    type Env = 
        {userRepo : Repositories.UserRepo}
    
    type App<'a> = ReaderT<Env, Result<'a, Err>>

//big module with SQL implementations and mappers
module PgSql = 
    open Npgsql.FSharp
    open Types.Domain
    
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
        | ["user_id", String userId; "user_email", String userEmail; 
           "user_password", String userPassword] -> 
            Some {UserId = userId;
                  UserEmail = userEmail;
                  UserPassword = userPassword}
        | _ -> None
    
    let getUserById id = 
        conn
        |> Sql.query 
               "select user_id,user_email,user_password from users where user_id = @userId"
        |> Sql.parameters ["userId", String id]
        |> Sql.executeTable
        |> Sql.mapEachRow userMapper
        |> List.tryHead
    
    let createUser user = 
        conn
        |> Sql.query 
               "insert into users(user_id,user_email,user_password) values(@userId, @userEmail, @userPassword)"
        |> Sql.parameters ["userId", String user.UserId;
                           "userEmail", String user.UserEmail;
                           "userPassword", String user.UserPassword]
        |> Sql.executeNonQuery
        |> ignore

//our main program
module Main = 
    open Types
    open Domain
    let getUser userId : App<User option> = 
        monad {let! env = ask
               return env.userRepo.getUserById userId}
    let insertNewUser : App<string> = 
        monad {
            let! env = ask
            let id = Guid.NewGuid().ToString()
            
            let newUser = 
                {UserId = id;
                 UserEmail = sprintf "%s@example.com" id;
                 UserPassword = "12345"}
            env.userRepo.createUser newUser
            return id
        }

    let run : App<unit> = 
        monad {
            let! id = insertNewUser
            let! user = getUser id
            printfn "%A" user
        }

//all bindings between contracts and implementations go here
module Injector = 
    open Types

    let impl = 
        {userRepo = 
            {getUserById = PgSql.getUserById;
             createUser = PgSql.createUser }}


[<EntryPoint>]
let main argv =
    printfn "%A" (ReaderT.run Main.run Injector.impl)
    0 // return an integer exit code
