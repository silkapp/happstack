#ifdef WOOT
To generate .html from this file run:

cpphs -DHsColour  --noline Examples.lhs  | HsColour -lit -css  | markdown --html4tags > examples.html

And remove the extra <p></p> around the <html></html> tags.
#endif

<a href="http://www.haskell.org/haskellwiki/Jmacro">JMacro</a> is a
library that makes it easy to generate javascript in Haskell. This is
useful even if are just trying to add a little bit of javascript code
to your HTML templates.

The syntax used by JMacro is almost identical to javascript. So, you
do not have to learn some special DSL to use it. In fact, JMacro can
work with most javascript you find in the wild.


 * syntax checking ensures that your javascript is syntactically valid
   at compile time. That eliminates many comman javascript errors and
   reduces development time

 * hygienic names and scoping automatically and transparently ensures
   that blocks of javascript code do not accidentally create variables
   and functions with conflicting names.

 * Antiquotation, Marshalling, and Shared scope make it easy to pass
   Haskell values into the javascript code. It also makes it easy to
   programmatically generate javascript code.

The happstack-jmacro library makes it easy to use JMacro with <a href="http://www.happstack.com/">Happstack</a> and HSP.

The following should get you started.

The JMacro library does not require any external
pre-processors. Instead it uses the magic of <a
href="http://haskell.org/haskellwiki/Quasiquotation">QuasiQuotation</a>.

So, we need to enabled the QuasiQuotes LANGUAGE extension:

> {-# LANGUAGE CPP, FlexibleInstances, GeneralizedNewtypeDeriving, TypeSynonymInstances, QuasiQuotes #-}

In this example we are also using HSX, which does require a
pre-processor. The following line will automatically run the
pre-processor for us (and also suppress warnings about orphan
instances):

> {-# OPTIONS_GHC -F -pgmFtrhsx -fno-warn-orphans #-}

Next we have a boatload if imports:

> import Control.Applicative ((<$>), optional)
> import Control.Monad (msum)
> import Control.Monad.State (StateT, evalStateT)
> import Control.Monad.Trans (liftIO)
> import qualified Data.Map as Map
> import Data.Maybe (fromMaybe)
> import Data.Unique
> import Happstack.Server
> import Happstack.Server.HSP.HTML    (defaultTemplate) -- ^ also imports ToMessage instance for XML type
> import Happstack.Server.JMacro      () -- ToMessage instance for JStat
> import HSP
> import HSP.ServerPartT ()           -- ^ XMLGenerator instance for ServerPartT
> import HSX.JMacro      (IntegerSupply(..), nextInteger') -- ^ EmbedAsChild and EmbedAsAttr instances for JStat
> import Language.Javascript.JMacro   
> import System.Random

In order to ensure that each &lt;script&gt; tag generates unique variables
names, we need a source of prefixes. Any easy way to do that is to
wrap the ServerPartT monad around a StateT monad that supplies
integers:

> type JMacroPart = ServerPartT (StateT Integer IO)

> instance IntegerSupply JMacroPart where
>     nextInteger = nextInteger'

The nextInteger' helper function has the type:

#ifdef HsColour
>  nextInteger' :: (MonadState Integer m) => m Integer
#endif

To use JMacroPart with simpleHTTP, we just evaluate the StateT monad:

> main :: IO ()
> main = simpleHTTP nullConf $ flatten handlers
>     where
>       flatten :: JMacroPart a -> ServerPartT IO a 
>       flatten = mapServerPartT (flip evalStateT 0)

Now that we have the scene set, we can actually look at some JMacro usage.

In this example we embed a single javascript block inside the page:

> helloJMacro :: JMacroPart Response
> helloJMacro =
>     toResponse <$> defaultTemplate "Hello JMacro" ()
>       <div>
>        <% [$jmacro| 
>            var helloNode = document.createElement('h1');
>            helloNode.appendChild(document.createTextNode("Hello, JMacro!"));
>            document.body.appendChild(helloNode);
>            |] %>
>       </div>

We do not need to specify the script tag explicitly, it will automatically be created for us. 

We can also use jmacro inside html attributes, such as onclick.

> helloAttr :: JMacroPart Response
> helloAttr =
>     toResponse <$> defaultTemplate "Hello JMacro" ()
>     <h1 style="cursor:pointer" onclick=[$jmacro| alert("that <tickles>!") |]>Click me!</h1>

Note that we do not have to worry about escaping the ", < or > in the
onclick handler. It is taken care of for us automatically!

So far, using jmacro with HSP looks almost exactly like using HSP with
out jmacro. That's actually pretty exciting. It mean that the mental
tax for using jmacro over straight javascript is very low.

Now let's look at an example of hygienic naming. Let's say we write
the following block of javascript code:

> clickMe :: JStat
> clickMe =
>     [$jmacro|
>
>     var clickNode = document.createElement('p');
>     clickNode.appendChild(document.createTextNode("Click me!"));
>     document.body.appendChild(clickNode);
>     var clickCnt = 0;
>     clickNode.setAttribute('style', 'cursor: pointer');
>     clickNode.onclick = function () { clickCnt++;
>                                       alert ('Been clicked ' + clickCnt + ' time(s).'); 
>                                     };
>     |]

That block of code tracks how many times you have clicked on the
'Click me!' text. It uses a global variable to keep track of the
number of clicks. Normally that would spell trouble. If we tried to
use that code twice on the same page, the instances would overwrite
each other.

But, jmacro automatically renames the variables for us so that the
names are unique. In the following code each 'Click me!" tracks its
counts separately:

> clickPart :: JMacroPart Response
> clickPart = 
>     toResponse <$> defaultTemplate "Hygienic Naming" ()
>                    <div>
>                     <h1>A Demo of Happstack+HSP+JMacro</h1>
>                     <% clickMe %>
>                     <% clickMe %>
>                    </div>

Of course, sometimes we want the code blocks to share a global
variable. We can easily do that by changing the line:

#ifdef HsColour
>   var clickCnt = 0;
#endif

to

#ifdef HsColour
>   var !clickCnt = 0;
#endif

Now all the copies of clickMe2 will share the same counter:

> clickMe2 :: JStat
> clickMe2 =
>     [$jmacro|
>
>     var clickNode = document.createElement('p');
>     clickNode.appendChild(document.createTextNode("Click me!"));
>     document.body.appendChild(clickNode);
>     var !clickCnt = 0;
>     clickNode.setAttribute("style", "cursor: pointer");
>     clickNode.onclick = function () { clickCnt++;
>                                       alert ('Been clicked ' + clickCnt + ' time(s).'); 
>                                     };
>     |]
>
> clickPart2 :: JMacroPart Response
> clickPart2 = 
>     toResponse <$> defaultTemplate "Hygienic Naming" ()
>                    <div>
>                     <h1>A Demo of Happstack+HSP+JMacro</h1>
>                     <% clickMe2 %>
>                     <% clickMe2 %>
>                    </div>

Often times we want to call some javascript code that contains some Haskell values spliced in. This can be done using `( )`.

> fortunePart :: JMacroPart Response
> fortunePart =
>     do let fortunes = ["Your will be cursed to write java for the rest of your days."
>                       , "Fortune smiles upon you, your future will be filled with lambdas"
>                       ]
>        n <- liftIO $ randomRIO (0, (length fortunes) - 1)
>             
>        toResponse <$> defaultTemplate "Fortune"
>                     <% [$jmacro|
>                         function !revealFortune(fortune)
>                         {
>                          var b = document.getElementById("button");
>                          b.setAttribute('disabled', 'disabled');
>                          var p = document.getElementById("fortune");
>                          p.appendChild(document.createTextNode(fortune));
>                         }
>                        |]
>                         %>
>                    <div>
>                     <h1>Your Fortune</h1>
>                     <p id="fortune"></p>
>                     <button id="button" onclick=[$jmacro| revealFortune(`(fortunes !! n)`); |]>Click to reveal your fortune</button>
>                    </div>

JMacro can embed 'primitives' such as Int, Bool, Char, String, etc, by default. But we can also embed other types by creating a ToJExpr instance for them. For example, let's say we create some types for reporting the weather:

> data Skies = Cloudy | Clear
>            deriving (Bounded, Enum, Eq, Ord, Read, Show)
>
> newtype Fahrenheit = Fahrenheit Double
>            deriving (Num, Enum, Eq, Ord, Read, Show, ToJExpr, Random)
>
> data Weather = Weather
>     { skies :: Skies
>     , temp  :: Fahrenheit
>     }
>     deriving (Eq, Ord, Read, Show)
>
> instance Random Skies where
>     randomR (lo, hi) g =
>        case randomR (fromEnum lo, fromEnum hi) g of
>          (c, g') -> (toEnum c, g')
>     random g = randomR (minBound, maxBound) g
>
> instance Random Weather where
>     randomR (Weather skiesLo tempLo, Weather skiesHi tempHi) g =
>         let (skies, g') = randomR (skiesLo, skiesHi) g
>             (temp, g'') = randomR (tempLo, tempHi) g'
>         in ((Weather skies temp), g'')
>     random g =
>         let (skies, g') = random g
>             (temp, g'') = random g'
>         in ((Weather skies temp), g'')

To pass these values into the generated javascript, we simply create a ToJExpr instance:

#ifdef HsColour
> class ToJExpr a where
>   toJExpr :: a -> JExpr
#endif

For Fahrenheit, we were actually able to derive the ToJExpr instance automatically (aka, deriving (ToJExpr)).

For Skies, we can just convert the values into javascript strings:

> instance ToJExpr Skies where
>     toJExpr = toJExpr . show
>

For the Weather type, we create a javascript object/hash/associative array:

> instance ToJExpr Weather where
>    toJExpr (Weather skies temp) = 
>        toJExpr (Map.fromList [ ("skies", toJExpr skies)
>                              , ("temp",  toJExpr temp)
>                              ])

Now we can splice a random weather report into our javascript:

> weatherPart :: JMacroPart Response
> weatherPart =
>     do weather <- liftIO $ randomRIO ((Weather minBound (-40)), (Weather maxBound 100))
>        toResponse <$> defaultTemplate "Weather Report" () 
>         <div>
>          <% [$jmacro|
>              var w = `(weather)`;
>              var p = document.createElement('p');
>              p.appendChild(document.createTextNode("The skies will be " + w.skies + " and the temperature will be " + w.temp.toFixed(1) + "°F"));
>              document.body.appendChild(p);
>              |] %>
>         </div>

ToJExpr has an instance for JSValue from the <a href="http://hackage.haskell.org/package/json-0.4.4">json library</a>. So, if your type already has a JSON istance, you can trivially create a ToJExpr instance for it:

#ifdef HsColour
> instance ToJExpr Foo where
>   toJExpr = toJExpr . showJSON
#endif

So far we have use jmacro to generate javascript that is embedded in HTML. We can also use it to create standalone javascript files.

First we have a script template that is parameterized by a greeting.

> externalJs :: String -> JStat
> externalJs greeting =
>     [$jmacro|
>      function !greet(noun) 
>      {
>        alert(`(greeting)` + ' ' + noun);
>      }
>      |]

Then we have a part with two sub-parts:

> externalPart :: JMacroPart Response
> externalPart = dir "external" $ msum [ 

If external/script.js is requested, then we check for a query string parameter 'greeting' and generate the script. 'toResponse' will automatically convert the script to a 'Response' and serve it with the content-type, "text/javascript; charset=UTF-8":

>             dir "script.js" $
>                do greeting <- optional $ look "greeting"
>                   ok $ toResponse $ externalJs (fromMaybe "hello" greeting)

Next we have an html page that includes the external script, and calls jmacro;

>          , toResponse <$> defaultTemplate "external" <script type="text/javascript" src="/external/script.js?greeting=Ahoy" />
>              <div>
>               <h1>Greetings</h1>
>               <button onclick=[$jmacro| greet('jmacro'); |]>Click for a greeting.</button>
>              </div>
>          ]

And little page that links to all the demos:

> demosPart :: JMacroPart Response
> demosPart =
>     toResponse <$> defaultTemplate "demos" ()
>                    <ul>
>                     <li><a href="/hello">Hello, JMacro</a></li>
>                     <li><a href="/attr">Hello, Attr</a></li>
>                     <li><a href="/clickMe">ClickMe</a></li>
>                     <li><a href="/clickMe2">ClickMe2</a></li>
>                     <li><a href="/weather">Weather</a></li>
>                     <li><a href="/fortune">Fortune</a></li>
>                     <li><a href="/external">External</a></li>
>                    </ul>

and our routes:

> handlers :: JMacroPart Response
> handlers = 
>    msum [ dir "hello" helloJMacro
>         , dir "attr" $ helloAttr
>         , dir "clickMe" $ clickPart
>         , dir "clickMe2" $ clickPart2
>         , dir "weather" $ weatherPart
>         , dir "fortune" $ fortunePart
>         , externalPart
>         , demosPart
>         ]

If you do not like having to use the StateT monad transformer to
generate names, there are other options. For example, we could use
Data.Unique to generate unique names:

#ifdef HsColour
> instance IntegerSupply (ServerPartT IO) where
>     nextInteger = fmap (fromIntegral . (`mod` 1024) . hashUnique) (liftIO newUnique)
#endif

This should be safe as long as you have less than 1024 different jmacro blocks on a single page.
