{ stdenv, lib, pkgs, buildInputs ? [ ], files ? [ ] }:
let
  theme = pkgs.fetchFromGitHub {
    owner = "jez";
    repo = "pandoc-markdown-css-theme";
    rev = "67c1d1cb8ac81b6cbb4144cc24ae429a195f0fca";
    sha256 = "sha256-KW90PICXohC6uPoFty9OpN+jUgNuS2hJCpiSCAWzAU0=";
  };

in stdenv.mkDerivation {
  name = "my-manual";

  buildInputs = buildInputs ++ [ pkgs.pandoc ];

  # files = [./foo.md ];
  phases = [ "installPhase" ];

  installPhase = ''
    mkdir -p $out/bin
    cp $files $out/bin
    cd $out/bin
    for file in $files; do
      output_file=$(cat $file | awk 'BEGIN { RS = "---" } NR==2' \
                    | awk -F': ' '/title:/ {print $2}' \
                    | tr -c '[:alnum:] ' '_' \
                    | sed 's/_\+$//;s/^_//;').html
      if [ -z "output_file" ]; then
        output_file="$(echo $file | xargs basename | awk -F'[-.]' '{printf "%s-%.6s.html\n", $(NF-1), $1}')"
      fi

      pandoc \
             --standalone \
             --from markdown \
             --to html5+smart \
             --toc \
             --template ${theme}/template.html5 \
             --css ${theme}/docs/css/theme.css \
             --css ${theme}/docs/css/skylighting-paper-theme.css \
             -s "$file" \
             -o "$output_file"
      echo "$output_file" >> $out/list.txt
    done
    mkdir -p $out/manual
    mv *.html $out/manual
  '';

  meta = with lib; {
    description = ''Convert Markdown files to HTML and save them in "manual"'';
    license = licenses.mit;
  };
}
