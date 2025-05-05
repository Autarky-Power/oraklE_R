# oRaklE 1.0.1 (2025‑05‑05)

## Bug fixes
* `decompose_load_data()` example no longer fails on **macOS** because of floating‑point precision differences.  
* `mid_term_future()` example no longer fails on **macOS** for the same reason.

## Documentation
* Added **NEWS.md** to track changes.  
* Updated the DOI for *Zimmermann & Ziel (2024)* from the preprint to the published article.

## Improvements
* Added more robust error handling to all **functions** that call the *date.nager.at* API; occasional site overloads were causing intermittent failures.

