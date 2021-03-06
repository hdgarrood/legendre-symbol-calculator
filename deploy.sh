#! /bin/bash -e

function check_status {
  if [[ `git status --porcelain | wc -l` == "0" ]]; then
      return 0
  else
      return 1
  fi
}

if check_status; then
    echo 'deploying...'

    BRANCH=`git rev-parse --abbrev-ref HEAD`

    pulp build --to dist/app.js
    tar czf app.tar.gz -C dist/ app.js index.html Legendre.jpg
    git checkout gh-pages
    tar xzf app.tar.gz

    if git commit --all --allow-empty -m deploying; then
      echo "> git push origin gh-pages"
      git push origin gh-pages
    else
      echo "Commit failed; not pushing."
    fi

    rm app.tar.gz

    echo "switching back to ${BRANCH}..."
    git checkout "$BRANCH"
else
    echo 'Your working tree is dirty or there are untracked files.'
    echo 'Commit or stash your changes first.'
    echo '`git status --porcelain` should not print anything.'
fi
