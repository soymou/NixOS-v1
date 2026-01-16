{ pkgs }:
with pkgs; [
  (hunspell.withDicts (dicts: [ dicts.en_US dicts.es_MX ]))
  nil
]
