import Text.Hakyll (hakyll)
import Text.Hakyll.File (directory)
import Text.Hakyll.Render (css, static, renderChain)
import Text.Hakyll.CreateContext (createPage)

main = hakyll "http://example.com" $ do
    directory css "css"
    directory static "images"
    directory static "docs" 
    directory static "html"
    render "about.rst"
    render "index.markdown"
    render "contact.markdown"
    render "writing.markdown"
    render "software.markdown"
    render "lsystem.markdown"
    render "bjpopray.markdown"
    render "imgtrans.markdown"
  where
    render = renderChain ["templates/default.html"]
           . createPage

