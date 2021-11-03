module HtmlTemplate where

import Data.List
import Prelude
import Data.Tuple

import Data.Array.NonEmpty as NA

-- 1. Layout
--   -> class style
-- 2. HtmlTag

arrayToList :: forall a. Array a -> List a
arrayToList = NA.fromFoldable

data Attribute = Attribute String String
instance showAttribute :: Show Attribute where 
  show (Attribute k v) = k <> "=\"" <> v <> "\""

data Layout = Layout {
  className :: String,
  style :: String,
  attrs :: List Attribute
}

instance semigroupLayout :: Semigroup Layout where
  append (Layout l1) (Layout l2) = Layout {
    className: l1.className <> " " <> l2.className,
    style: l1.style <> ";" <> l2.style,
    attrs: l1.attrs <> l2.attrs
  }

instance monoidLayout :: Monoid Layout where
  mempty = Layout {
    className: "",
    style: "",
    attrs: Nil
  }


------ 定义基础
data MyElem
  = Div Layout (List MyElem)
  -- | Span Layout String
  -- | Row Layout (List MyElem)
  -- | Col Layout (List MyElem)
  -- | Img Layout
  -- | Input Layout


render :: MyElem -> String
render (Div l children) = "<div" <> renderLayout l <> ">" <> (fold $ render <$> children) <> "</div>"


renderLayout :: Layout -> String
renderLayout (Layout {className: c, style: s, attrs: a}) = 
  foldl (\pv cv -> pv <> " " <> cv) "" $ show <$> list
  where 
    list = (Attribute "class" c) : (Attribute "style" s) : a



--- 定义方便的构造方法

data LayoutPrime
  = ClassName String
  | Style String
  | Attr String String

div :: Array LayoutPrime -> Array MyElem -> MyElem
div layouts children = Div Layout {
  className: className,
  style: style,
  attrs: attrs
} (NA.fromFoldable children)
  where 
    show' (ClassName cn) = Tuple 1 cn
    show' (Style s) = Tuple 2 s
    show' (Attr )
    isClassName (ClassName _) = true
    isClassName _ = false

    isStyle (Style _) = true
    isStyle _ = false

    isAttr (Attr _ _) = true
    isAttr _ = false 

    className = 
       . (filter isClass)

test1 :: String
test1 = render testParent



-- import Data.Array (fold, foldMap, foldl)
-- import Data.List (List(..), (:))
-- import Data.List(fold) as L

-- data Alignment = Start | Center | End

-- data Layout = ClassName String
--   | Style String
--   | Attribute String String

-- data LayoutData = LayoutData {
--   classNames :: List String,
--   style :: String,
--   attributes :: List String
-- }

-- instance showLayoutData :: Show LayoutData where
--   show (LayoutData ld) = "class=\"" <> L.fold ld.classNames <> "\" style=\"" <> ld.style <> "\" "  <> L.fold ld.attributes  

-- emptyLayoutData :: LayoutData
-- emptyLayoutData = LayoutData { classNames: Nil, style: "", attributes: Nil }

-- instance semigroupLayoutData :: Semigroup LayoutData where
--   append :: LayoutData -> LayoutData -> LayoutData
--   append (LayoutData lda) (LayoutData ldb) = LayoutData {classNames: lda.classNames <> ldb.classNames, style: lda.style <> ldb.style, attributes: lda.attributes <> ldb.attributes} 

-- instance monoidLayoutData :: Monoid LayoutData where
--   mempty :: LayoutData
--   mempty = emptyLayoutData


-- type Layouts = Array Layout

-- newtype Callback = Effect Unit

-- data Element = Row Layouts (Array Element)
--   | Col Layouts (Array Element)
--   | A Layouts (Array Element)
--   | Button Layouts (Array Element) Callback
--   | Image Layouts String
--   | Text String
--   | Div Layouts (Array Element)

-- showLayout :: Layout -> LayoutData
-- showLayout (ClassName x) = LayoutData { classNames: x:Nil, style:"", attributes: Nil }
-- showLayout (Style s) = LayoutData { classNames: Nil, style: s, attributes: Nil }
-- showLayout (Attribute name value) = LayoutData{ classNames: Nil, style: "", attributes: (name <> "=\"" <> value <> "\""):Nil }

-- showLayouts :: Array Layout -> LayoutData
-- showLayouts = foldMap showLayout

-- renderList :: List Element -> String
-- renderList l = L.fold $ render <$> l

-- colLayoutData :: LayoutData 
-- colLayoutData = LayoutData {
--   classNames: Nil,
--   style: "",
--   attributes: Nil
-- }

-- rowLayoutData :: LayoutData 
-- rowLayoutData = LayoutData {
--   classNames: Nil,
--   style: "",
--   attributes: Nil
-- }

-- render :: Element -> String
-- render (Col layouts children) = let ld = showLayouts layouts in render (Div (ld <> colLayoutData) children)
-- render (Row layouts children) = 
--   "<div style=\"row\">" <> renderList children <> "</div>"
-- render (A layouts children) = 
--   "<div style=\"\">" <> renderList children <> "</div>"
-- render (Button layouts children cb) = 
--   "<div style=\"\" onclick=\"cb\">" <> renderList children <> "</div>"
-- render (Image layouts imgsrc) = "<img>"
-- render (Text t) = t
-- render (Div layouts children) = "<div style=\"\">" <> renderList children <> "</div>"

-- data HElement 
--   = HDiv LayoutData (Array HElement)
--   | HSpan LayoutData (Array HElement)
--   | HImage LayoutData LayoutData
--   | HText String
--   | HButton LayoutData (Array HElement)
