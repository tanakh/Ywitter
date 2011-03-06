import Controller (withYwitter)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = withYwitter $ run 3000
