module CommonImports where

import Main ( lookupSetting, makePool )

withApp = before $ do
    env  <- lookupSetting "ENV" Test
    pool <- makePool env
    let cfg = Config { getPool = pool, getEnv = env }
