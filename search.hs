-- For Scotty
{-# LANGUAGE OverloadedStrings #-}

-- For Scotty
import Web.Scotty
import Data.Monoid (mconcat)

-- For Sphinx/custom code wrapping around Sphinx
import qualified Text.Search.Sphinx as Sphinx
import qualified Text.Search.Sphinx.Types as SphinxT
import qualified Text.Search.Sphinx.ExcerptConfiguration as SphinxE
-- Part of MissingH
import Data.String.Utils (replace)

configGeneric = Sphinx.defaultConfig { Sphinx.host = "db.csh.rit.edu", Sphinx.port = 9312, Sphinx.mode = SphinxT.All }
configBoolean = configGeneric { Sphinx.mode = SphinxT.Boolean }
configExcerpt = SphinxE.defaultConfig { SphinxE.port = 9312, SphinxE.host = "db.csh.rit.edu" }


main = scotty 3000 $ do
	get "/" $
		file "main_search_page.html"
	get "csh_bling_logo.png" $
		file "csh_bling_logo.png"
	get "/search/all/:query" $ do
		query <- param "query"
		text "general query"
	get "/search/wiki/:query" $ do
		query <- param "query"
		text "wiki query"
	get "/search/news/:query" $ do
		query <- param "query"
		text "news query"

-- A variety of query preprocessing functions in order to change SphinxT.Boolean match mode into the advanced query syntax that people are more used to (a la Google/Bing/Yahoo/??)
stripParens str = [c | c <- str, c/='(' && c/=')']

oddQuotes str = length [ c | c<-str, c=='"'] `mod` 2
quoteRebalance str | oddQuotes str == 1 = quoteRebalance $ str++"\""
		   | oddQuotes str == 0 = str

customEscapeString str = unEscapeQuotes $ Sphinx.escapeString str
-- It's confusing, this is actually replacing \" with ", but there is Houdini levels of escaping going on
unEscapeQuotes str = replace "\\\"" "\"" str

queryGeneric db queryString = Sphinx.query configBoolean db queryString
queryAll queryString = queryGeneric "*" queryString
queryWiki queryString = queryGeneric "wiki-main" queryString
queryNews queryString = queryGeneric "webnews" queryString

