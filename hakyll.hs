import Text.Hakyll (hakyll)
import Text.Hakyll.File (directory)
import Text.Hakyll.Render (css, static, renderChain)
import Text.Hakyll.CreateContext (createPage)

main = hakyll "http://example.com" $ do
    directory css "css"
    static "images/bernie.bw.face.jpg"
    render "about.rst"
    render "index.markdown"
    render "code.lhs"
  where
    render = renderChain ["templates/default.html"]
           . createPage

