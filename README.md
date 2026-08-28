# scotty-form

[![Hackage](https://img.shields.io/hackage/v/scotty-form.svg)](https://hackage.haskell.org/package/scotty-form)

Scotty integration for [ditto](https://github.com/goolord/ditto) formlets, using [lucid](https://hackage.haskell.org/package/lucid) for HTML rendering.

## Usage

```haskell
import Web.Scotty
import Web.Scotty.Form
import Ditto.Lucid
import Ditto.Lucid.Named

main :: IO ()
main = scotty 3000 $
  get "/user" $ do
    (res, html') <- simpleDittoGET "/user" formUser
    html $ renderText html'
```

See the `example/` directory for a complete demo.

## Development

By default, `cabal.project` resolves `ditto` and `ditto-lucid` from Hackage.

For local development against git checkouts, copy the example local project file:

```bash
cp cabal.project.local.example cabal.project.local
# edit paths if your layout differs (sibling vs in-repo checkouts)
cabal build all
cabal run scotty-form-example
```

Typical sibling layout:

```
Dev/
  ditto/
  ditto-lucid/
  scotty-form/    # cabal.project.local points at ../ditto and ../ditto-lucid
```

## License

MIT — see [LICENSE](scotty-form/LICENSE).
