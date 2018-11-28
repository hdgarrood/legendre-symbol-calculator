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
    tar czf app.tar.gz -C dist/ app.js index.html
    git checkout gh-pages
    tar xzf app.tar.gz
    git commit --all -m deploying
    rm app.tar.gz

    echo "switching back to ${BRANCH}..."
    git checkout "$BRANCH"
else
    echo 'Your working tree is dirty or there are untracked files.'
    echo 'Commit or stash your changes first.'
    echo '`git status --porcelain` should not print anything.'
fi
