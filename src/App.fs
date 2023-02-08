module App
open FsharpMyExtension
open FsharpMyExtension.Either
open Microsoft.Extensions.Logging
open System.Threading.Tasks

open Types
open Extensions

let botEventId = new EventId(42, "Bot-Event")

let cmd pstart (client: DSharpPlus.DiscordClient) (e: DSharpPlus.EventArgs.MessageCreateEventArgs) =
    let authorId = e.Author.Id
    let botId = client.CurrentUser.Id

    if authorId <> botId then
        match pstart botId e.Message.Content with
        | Right res ->
            match res with
            | CommandParser.Pass -> ()

            | CommandParser.Unknown -> ()

            | CommandParser.MessageCreateEventHandler exec ->
                exec (client, e)

        | Left x ->
            awaiti (client.SendMessageAsync (e.Channel, (sprintf "Ошибка:\n```\n%s\n```" x)))

let initBotModules postmanType (db: MongoDB.Driver.IMongoDatabase) =
    [|
        Mail.Main.create postmanType db
    |]

open MongoDB.Driver
let initDb () =
    let dbConnection = getEnvironmentVariable "DbConnection"

    let settings =
        MongoClientSettings.FromConnectionString (dbConnection)

    let client = new MongoClient(settings)
    let database =
        let dataBaseName =
            getEnvironmentVariable "DataBaseName"

        client.GetDatabase(dataBaseName)

    database

open Saturn
open Giraffe

let startServer () =
    let port =
        match System.Environment.GetEnvironmentVariable("PORT") with
        | null -> uint16 8088
        | port -> uint16 port
    let publicPath = System.IO.Path.GetFullPath "./public"

    let app =
      application {
        use_static publicPath
        use_router (text "Hello World from Saturn")
    #if !DEBUG
        disable_diagnostics
    #endif
        url ("http://0.0.0.0:" + port.ToString() + "/")
      }

    run app

[<EntryPoint>]
let main argv =
    let getBotToken next =
        let tokenEnvVar = "BotToken"
        match tryGetEnvironmentVariable tokenEnvVar with
        | None ->
            printfn "Environment variable `%s` is not set!" tokenEnvVar
            1
        | Some token ->
            next token

    let getPostmanType next =
        let postmanTypeEnvVar = "PostmanType"
        match tryGetEnvironmentVariable postmanTypeEnvVar with
        | None ->
            printfn "Environment variable `%s` is not set!" postmanTypeEnvVar
            2
        | Some var ->
            match Mail.Main.PostmanType.tryDeserialize var with
            | Error errMsg ->
                printfn "%s" errMsg
                2
            | Ok postmanType ->
                next postmanType

    getBotToken <| fun token ->
    getPostmanType <| fun postmanType ->
    let config = DSharpPlus.DiscordConfiguration()

    config.set_Token token
    config.set_TokenType DSharpPlus.TokenType.Bot
    config.set_AutoReconnect true
    config.set_Intents (
        DSharpPlus.DiscordIntents.AllUnprivileged
        ||| DSharpPlus.DiscordIntents.GuildMembers
        ||| DSharpPlus.DiscordIntents.GuildPresences
    )

    let client = new DSharpPlus.DiscordClient(config)

    let database = initDb ()
    let botModules = initBotModules postmanType database

    botModules
    |> Shared.BotModule.bindToClientsEvents
        CommandParser.initCommandParser
        CommandParser.start
        cmd
        (fun _ _ -> ())
        client

    client.add_Ready(Emzi0767.Utilities.AsyncEventHandler (fun client readyEventArgs ->
        client.Logger.LogInformation(botEventId, "Client is ready to process events.")

        Task.CompletedTask
    ))

    client.add_ClientErrored(Emzi0767.Utilities.AsyncEventHandler (fun client e ->
        client.Logger.LogError(botEventId, e.Exception, "Exception occured", [||])

        Task.CompletedTask
    ))

    client.add_GuildDownloadCompleted(Emzi0767.Utilities.AsyncEventHandler (fun client e ->
        let activity = DSharpPlus.Entities.DiscordActivity("Собираю новогодние поздравления!")
        awaiti <| client.UpdateStatusAsync(activity)

        Task.CompletedTask
    ))

    awaiti <| client.ConnectAsync()

    // awaiti <| Task.Delay -1
    startServer ()

    0
