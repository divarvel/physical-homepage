'use strict'

let compile = run({
  sh: 'stack build',
  watch: 'app/**'
})

let server = runServer({
  httpPort,
  env: { "PORT": httpPort },
  sh: `./.stack-work/install/*/*/*/bin/spock-exe`
}).dependsOn(compile)

proxy(server, 8080).dependsOn(compile)
