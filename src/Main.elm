module Main exposing (init, update, view, Model, Msg)

import AWS.Auth as Auth
import AuthAPI
import Browser
import Html
import Process
import Task
import Update3
import Element exposing (Element)
import Element.Input as Input


type Model
    = Error String
    | Restoring InitializedModel
    | Initialized InitializedModel


type alias InitializedModel =
    { auth : Auth.Model
    , session : AuthAPI.Status Auth.AuthExtensions Auth.Challenge
    , username : String
    , password : String
    , passwordVerify : String
    }


type Msg
    = AuthMsg Auth.Msg
    | InitialTimeout
    | LogIn
    | LogOut
    | RespondWithNewPassword
    | TryAgain
    | Refresh
    | UpdateUsername String
    | UpdatePassword String
    | UpdatePasswordVerificiation String


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }

-- Initialization


{-| Initializes the application state by setting it to the default Auth state
of LoggedOut.
Requests that an Auth refresh be performed to check what the current
authentication state is, as the application may be able to re-authenticate
from a refresh token held as a cookie, without needing the user to log in.
-}
init : flags -> ( Model, Cmd Msg )
init _ =
    let
        authInitResult =
            Auth.api.init
                { clientId = "4dc0rodjg7q69rovjnnuuuam2b"
                , region = "us-east-1"
                , userIdentityMapping =
                    Just
                        { userPoolId = "us-east-1_sWN5Fmrjp"
                        , identityPoolId = "us-east-1:6227f5bb-9338-4fa6-b209-98644482f5d0"
                        , accountId = "432853825205"
                        }
                }
    in
    case authInitResult of
        Ok authInit ->
            ( Initialized
                { auth = authInit
                , session = AuthAPI.LoggedOut
                , username = ""
                , password = ""
                , passwordVerify = ""
                }
            , Process.sleep 1000 |> Task.perform (always InitialTimeout)
            )

        Err errMsg ->
            ( Error errMsg, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case model of
        Error _ ->
            ( model, Cmd.none )

        Restoring initModel ->
            updateInitialized action initModel
                |> Tuple.mapFirst Initialized

        Initialized initModel ->
            updateInitialized action initModel
                |> Tuple.mapFirst Initialized


updateInitialized : Msg -> InitializedModel -> ( InitializedModel, Cmd Msg )
updateInitialized action model =
    case Debug.log "msg" action of
        AuthMsg msg ->
            Update3.lift .auth (\x m -> { m | auth = x }) AuthMsg Auth.api.update msg model
                |> Update3.evalMaybe (\status -> \nextModel -> ( { nextModel | session = status }, Cmd.none )) Cmd.none

        InitialTimeout ->
            ( model, Auth.api.refresh |> Cmd.map AuthMsg )

        LogIn ->
            ( model, Auth.api.login { username = model.username, password = model.password } |> Cmd.map AuthMsg )

        RespondWithNewPassword ->
            case model.password of
                "" ->
                    ( model, Cmd.none )

                newPassword ->
                    ( model, Auth.api.requiredNewPassword newPassword |> Cmd.map AuthMsg )

        TryAgain ->
            ( clear model, Auth.api.unauthed |> Cmd.map AuthMsg )

        LogOut ->
            ( clear model, Auth.api.logout |> Cmd.map AuthMsg )

        Refresh ->
            ( model, Auth.api.refresh |> Cmd.map AuthMsg )

        UpdateUsername str ->
            ( { model | username = str }, Cmd.none )

        UpdatePassword str ->
            ( { model | password = str }, Cmd.none )

        UpdatePasswordVerificiation str ->
            ( { model | passwordVerify = str }, Cmd.none )


clear : InitializedModel -> InitializedModel
clear model =
    { model | username = "", password = "", passwordVerify = "" }



-- View


view : Model -> Browser.Document Msg
view model = {
    title = "Elm Auth AWS Example",
    body = [Element.layout [] (viewBody model)]
    }

viewBody model = case model of
    Error errMsg -> errorView errMsg
    Restoring initModel -> initialView
    Initialized initModel -> initializedView initModel


errorView errMsg = Element.text errMsg


initializedView model =
    case model.session of
        AuthAPI.LoggedOut -> loginView model
        AuthAPI.Failed -> notPermittedView model
        AuthAPI.LoggedIn state -> authenticatedView model state
        AuthAPI.Challenged Auth.NewPasswordRequired ->
            requiresNewPasswordView model


initialView : Element Msg
initialView = Element.text "Attempting to restore authentication using a local refresh token."


loginView : InitializedModel -> Element Msg
loginView model = Element.column [] [
        Element.text "Login",
        Input.text [] {
            onChange = UpdateUsername,
            text = model.username,
            placeholder = Nothing,
            label = Input.labelLeft [] (Element.text "Username")
        },
        Input.currentPassword [] {
            onChange = UpdatePassword,
            text = model.password,
            placeholder = Nothing,
            label = Input.labelLeft [] (Element.text "Password"),
            show = False
        },
        Input.button [] { onPress = Just LogIn, label = Element.text "Log In" }
    ]

notPermittedView : InitializedModel -> Element Msg
notPermittedView model = Element.column [] [
    Element.text "Not Authorized",
    Input.text [] {
        onChange = UpdateUsername,
        text = model.username,
        placeholder = Nothing,
        label = Input.labelLeft [] (Element.text "Username")
    },
    Input.currentPassword [] {
        onChange = UpdatePassword,
        text = model.password,
        placeholder = Nothing,
        label = Input.labelLeft [] (Element.text "Password"),
        show = False
    },
    Input.button [] {
        onPress = Just TryAgain,
        label = Element.text "Try Again" }
    ]


authenticatedView : { a | username : String, auth : Auth.Model } -> { scopes : List String, subject : String } -> Element Msg
authenticatedView model user = Element.column [] [
    Element.text "Authenticated",
    Element.text ("Logged In As:" ++ model.username),
    Element.text ("With Id:" ++ user.subject),
    Element.text ("With Permissions:" ++ Debug.toString user.scopes),
    case Auth.api.getAWSCredentials model.auth |> Debug.log "credentials" of
        Just creds -> Element.text "With AWS access credentials."
        Nothing -> Element.none,
    Input.button [] {
        onPress = Just LogOut,
        label = Element.text "Log Out" },
    Input.button [] {
        onPress = Just Refresh, label = Element.text "Refresh" }
    ]


requiresNewPasswordView : { a | password : String, passwordVerify : String } -> Element Msg
requiresNewPasswordView model = Element.column [] [
    Element.text "New Password Required",
    Input.newPassword [] {
        onChange = UpdatePassword,
        text = model.password,
        placeholder = Nothing,
        label = Input.labelLeft [] (Element.text "Password"),
        show = False
    },
    Input.newPassword [] {
        onChange = UpdatePasswordVerificiation,
        text = model.passwordVerify,
        placeholder = Nothing,
        label = Input.labelLeft [] (Element.text "Password Confirmation"),
        show = False
    },
    Input.button [] {
        onPress = Just RespondWithNewPassword,
        label = Element.text "Set Password" }
    ]
