module SmSnEta where

main = putStrLn "Find SmSnEta in the compiled bytecode!"

foreign export java "@static net.fortytwo.smsn.SmSnEta.tryme" main :: IO ()
