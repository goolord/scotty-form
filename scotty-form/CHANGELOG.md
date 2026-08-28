# Changelog

## 0.5.0.0 — 2026-08-27

### Breaking
- Require `scotty >= 0.20` (drops `ScottyError` and `ActionT e m`; use `ActionT m`)
- `ScottyForm e m a` renamed to `ScottyForm m a` in `Web.Scotty.Trans.Form`
- `Param` values are strict `Text` (scotty >= 0.20); `liftParser` now takes strict `Text` parsers (same as `liftParser'`)
- `Environment` instance for `ActionT` requires `MonadUnliftIO` (needed for `formParams`)

### Changed
- Compatible with `ditto` 0.4 and 0.5 (ditto 0.5 is backwards compatible)
- Replace deprecated `params` with `pathParams`, `queryParams`, and `formParams`
- Prefer GHC 9.10+ (`GHC2024`) with fallback to `GHC2021` / `Haskell2010`
- Widen dependency bounds (`base >= 4.14`, `text >= 1.2`)
- Move language extensions into cabal `default-extensions`
- Modern cabal format (`cabal-version: 3.8`)

### Added
- GitHub Actions CI
- `cabal.project.local.example` for local `ditto` / `ditto-lucid` development
