module WeatherService.Sitemap (Sitemap (..)) where

import Data.Text (Text)

data Sitemap
    = Date Text
    | Update Text Text
    | Range Text Text
    | Max Text Text
    | Above Text
