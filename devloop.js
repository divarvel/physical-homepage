'use strict'

let compile = run({
  sh: 'stack build',
  watch: 'app/**'
})

let server = runServer({
  httpPort,
  env: { "PORT": httpPort },
  sh: `./.stack-work/install/x86_64-osx/lts-8.2/8.0.2/bin/spock-exe`
})

proxy(server, 8080).dependsOn(compile)
