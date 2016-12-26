module App exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Process
import Task
import Time exposing (Time, millisecond)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Mouse exposing (..)


type Branch
    = None
    | Left
    | Right


type alias CalcResult =
    { nextRight : Float
    , nextLeft : Float
    , a : Float
    , b : Float
    }


type Pythagoras
    = Pythagoras
        { branch : Branch
        , lvl : Int
        , x : Float
        , y : Float
        , w : Float
        , left : Maybe Pythagoras
        , right : Maybe Pythagoras
        }


type alias Model =
    { heightFactor : Float
    , lean : Float
    , currentMax : Int
    , memoizedCalcs : Dict String CalcResult
    , tree : Maybe Pythagoras
    }


initModel : Model
initModel =
    { heightFactor = 0
    , lean = 0
    , currentMax = 0
    , memoizedCalcs = Dict.empty
    , tree = Nothing
    }



-- CONSTANTS


svgWidth : Float
svgWidth =
    1200


svgHeight : Float
svgHeight =
    600


baseWidth : Float
baseWidth =
    80


maximumDepth : Int
maximumDepth =
    11



-- MATH AND STRING UTILITIES


toDegrees : Float -> Float
toDegrees radians =
    180.0 * radians / Basics.pi


roundTo : Int -> Float -> Int
roundTo places =
    let
        factor =
            toFloat (10 ^ places)
    in
        (*) factor >> round



-- COLORS (from d3)


type alias ColorRamp =
    ( Array String, String )


viridis : ColorRamp
viridis =
    let
        colors =
            "44015444025645045745055946075a46085c460a5d460b5e470d60470e6147106347116447136548146748166848176948186a481a6c481b6d481c6e481d6f481f70482071482173482374482475482576482677482878482979472a7a472c7a472d7b472e7c472f7d46307e46327e46337f463480453581453781453882443983443a83443b84433d84433e85423f854240864241864142874144874045884046883f47883f48893e49893e4a893e4c8a3d4d8a3d4e8a3c4f8a3c508b3b518b3b528b3a538b3a548c39558c39568c38588c38598c375a8c375b8d365c8d365d8d355e8d355f8d34608d34618d33628d33638d32648e32658e31668e31678e31688e30698e306a8e2f6b8e2f6c8e2e6d8e2e6e8e2e6f8e2d708e2d718e2c718e2c728e2c738e2b748e2b758e2a768e2a778e2a788e29798e297a8e297b8e287c8e287d8e277e8e277f8e27808e26818e26828e26828e25838e25848e25858e24868e24878e23888e23898e238a8d228b8d228c8d228d8d218e8d218f8d21908d21918c20928c20928c20938c1f948c1f958b1f968b1f978b1f988b1f998a1f9a8a1e9b8a1e9c891e9d891f9e891f9f881fa0881fa1881fa1871fa28720a38620a48621a58521a68522a78522a88423a98324aa8325ab8225ac8226ad8127ad8128ae8029af7f2ab07f2cb17e2db27d2eb37c2fb47c31b57b32b67a34b67935b77937b87838b9773aba763bbb753dbc743fbc7340bd7242be7144bf7046c06f48c16e4ac16d4cc26c4ec36b50c46a52c56954c56856c66758c7655ac8645cc8635ec96260ca6063cb5f65cb5e67cc5c69cd5b6ccd5a6ece5870cf5773d05675d05477d1537ad1517cd2507fd34e81d34d84d44b86d54989d5488bd6468ed64590d74393d74195d84098d83e9bd93c9dd93ba0da39a2da37a5db36a8db34aadc32addc30b0dd2fb2dd2db5de2bb8de29bade28bddf26c0df25c2df23c5e021c8e020cae11fcde11dd0e11cd2e21bd5e21ad8e219dae319dde318dfe318e2e418e5e419e7e419eae51aece51befe51cf1e51df4e61ef6e620f8e621fbe723fde725"

        n =
            (String.length colors) // 6

        ramp =
            List.range 0 (n - 1)
                |> List.map (\i -> "#" ++ (String.slice (i * 6) ((i + 1) * 6) colors))
                |> Array.fromList

        def =
            "#" ++ (String.slice ((n - 1) * 6) (n * 6) colors)
    in
        ( ramp, def )


interpolateColor : Float -> ColorRamp -> String
interpolateColor f ( ramp, def ) =
    let
        n =
            Array.length ramp

        i =
            floor ((toFloat n) * (Basics.max (Basics.min f 1) 0))
    in
        Array.get i ramp |> Maybe.withDefault def



-- CACHED CALCULATIONS


memoKey : Float -> Float -> Float -> String
memoKey w heightFactor lean =
    String.join "." (List.map ((roundTo 2) >> toString) [ w, heightFactor, lean ])


memoizedCalc : Float -> Float -> Float -> Model -> ( Model, CalcResult )
memoizedCalc w heightFactor lean model =
    let
        key =
            memoKey w heightFactor lean
    in
        case Dict.get key model.memoizedCalcs of
            Just result ->
                ( model, result )

            Nothing ->
                let
                    h =
                        heightFactor * w

                    l =
                        w * (0.5 - lean)

                    r =
                        w * (0.5 + lean)

                    nextLeft =
                        sqrt ((h ^ 2) + (l ^ 2))

                    nextRight =
                        sqrt ((h ^ 2) + (r ^ 2))

                    a =
                        toDegrees (atan2 h l)

                    b =
                        toDegrees (atan2 h r)

                    result_ =
                        { nextRight = nextRight
                        , nextLeft = nextLeft
                        , a = a
                        , b = b
                        }
                in
                    ( { model | memoizedCalcs = Dict.insert key result_ model.memoizedCalcs }, result_ )



-- COMMANDS


{-| Increase the recursion depth every 500 milliseconds.
-}
next : Model -> ( Model, Cmd Msg )
next model =
    if model.currentMax < maximumDepth then
        let
            nextModel =
                { model | currentMax = model.currentMax + 1 }

            sleepNext =
                Process.sleep (500 * millisecond)
                    |> Task.attempt (\_ -> Next)
        in
            ( buildTree nextModel, sleepNext )
    else
        ( model, Cmd.none )


{-| Rebuild the tree based on mouse coordinates.
-}
mouseMoved : Int -> Int -> Model -> ( Model, Cmd Msg )
mouseMoved x y model =
    let
        scaleFactor y =
            (((Basics.max (Basics.min y svgHeight) 0) * -0.8) / svgHeight) + 0.8

        scaleLean x =
            (((Basics.max (Basics.min x svgWidth) 0) * -1.0) / svgWidth) + 0.5

        nextModel =
            { model | heightFactor = scaleFactor (toFloat y), lean = scaleLean (toFloat x) }
    in
        ( buildTree nextModel, Cmd.none )



-- INIT / UPDATE


type Msg
    = NoOp
    | Next
    | Position Int Int


init : ( Model, Cmd Msg )
init =
    update Next initModel


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Next ->
            let
                _ =
                    Debug.log "Next" ""
            in
                next model

        Position x y ->
            mouseMoved x y model

        _ ->
            ( model, Cmd.none )



-- FRACTAL DATA


buildTree : Model -> Model
buildTree model =
    let
        ( model_, fullTree ) =
            growTree ( { model | tree = Nothing }, initTree model )
    in
        { model_ | tree = fullTree }


{-| 0-depth fractal data
-}
initTree : Model -> Pythagoras
initTree model =
    Pythagoras
        { branch = None
        , lvl = 0
        , x = (svgWidth - baseWidth) / 2
        , y = svgHeight - baseWidth
        , w = baseWidth
        , left = Nothing
        , right = Nothing
        }


{-| n+1-depth fractal data
-}
growTree : ( Model, Pythagoras ) -> ( Model, Maybe Pythagoras )
growTree ( model, Pythagoras pyt ) =
    if pyt.lvl >= model.currentMax || pyt.w < 1 then
        ( model, Just <| Pythagoras pyt )
    else
        let
            ( model1, result ) =
                -- lookup or create cached calculation
                memoizedCalc pyt.w model.heightFactor model.lean model

            ( model2, left ) =
                -- recursive descent of left child
                growTree
                    ( model1
                    , Pythagoras
                        { branch = Left
                        , lvl = pyt.lvl + 1
                        , x = 0
                        , y = -result.nextLeft
                        , w = result.nextLeft
                        , left = Nothing
                        , right = Nothing
                        }
                    )

            ( model3, right ) =
                -- recursive descent of right child
                growTree
                    ( model2
                    , Pythagoras
                        { branch = Right
                        , lvl = pyt.lvl + 1
                        , x = pyt.w - result.nextRight
                        , y = -result.nextRight
                        , w = result.nextRight
                        , left = Nothing
                        , right = Nothing
                        }
                    )

            -- update parent
            pyt_ =
                { pyt | left = left, right = right }
        in
            ( model3, Just (Pythagoras pyt_) )



-- VIEW


view : Model -> Svg Msg
view model =
    svg
        [ version "1.1"
        , x "0"
        , y "0"
        , width (toString svgWidth)
        , height (toString svgHeight)
        , viewBox (String.join " " [ "0", "0", toString svgWidth, toString svgHeight ])
        , Svg.Attributes.style "border: 1px solid lightgray"
        ]
        (viewTree model)


viewTree : Model -> List (Svg Msg)
viewTree model =
    case model.tree of
        Nothing ->
            []

        Just pyt ->
            [ viewPythagoras pyt model ]


viewPythagoras : Pythagoras -> Model -> Svg Msg
viewPythagoras (Pythagoras pyt) model =
    let
        key =
            memoKey pyt.w model.heightFactor model.lean

        result =
            Dict.get key model.memoizedCalcs
    in
        case result of
            Nothing ->
                g [] []

            Just r ->
                -- recursive descent of fractal model
                viewPythagoras_ r (Pythagoras pyt) model


viewPythagoras_ : CalcResult -> Pythagoras -> Model -> Svg Msg
viewPythagoras_ r (Pythagoras pyt) model =
    let
        rotate =
            case pyt.branch of
                Left ->
                    " rotate(" ++ (toString -r.a) ++ " 0 " ++ (toString pyt.w) ++ ")"

                Right ->
                    " rotate(" ++ (toString r.b) ++ " " ++ (toString pyt.w) ++ " " ++ (toString pyt.w) ++ ")"

                None ->
                    ""

        xform =
            "translate(" ++ (toString pyt.x) ++ " " ++ (toString pyt.y) ++ ")" ++ rotate

        childLeft =
            case pyt.left of
                Nothing ->
                    []

                Just l ->
                    -- recursive descent of left child
                    [ viewPythagoras l model ]

        childRight =
            case pyt.right of
                Nothing ->
                    []

                Just r ->
                    -- recursive descent of right child
                    [ viewPythagoras r model ]

        fillColor =
            interpolateColor ((toFloat pyt.lvl) / (toFloat model.currentMax)) viridis

        children =
            [ rect
                [ width (toString pyt.w)
                , height (toString pyt.w)
                , x "0"
                , y "0"
                , fill fillColor
                , stroke "white"
                ]
                []
            ]
                ++ childLeft
                ++ childRight
    in
        g [ transform xform ] children



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Mouse.moves (\{ x, y } -> Position x y)
