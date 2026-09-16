'use strict';
// One graph and watcher per Emacs-owned process. Stdout is JSONL only.
const readline = require('node:readline');
console.log = console.error;
const { CodeGraph } = require(process.argv[2]);
const root = process.argv[3];
let graph;
let closing = false;
const send = value => process.stdout.write(JSON.stringify(value) + String.fromCharCode(10));
function status() {
  return {
    root, pid: process.pid,
    watching: graph.isWatching(),
    degraded: graph.isWatcherDegraded(),
    degradedReason: graph.getWatcherDegradedReason(),
    pendingFiles: graph.getPendingFiles(),
    indexing: graph.isIndexing(),
    indexState: graph.getIndexState(),
    lastIndexedAt: graph.getLastIndexedAt(),
  };
}
function close() {
  if (closing) return;
  closing = true;
  if (graph) graph.close();
  process.exit(0);
}
process.on('SIGTERM', close);
process.on('SIGINT', close);
async function start() {
  graph = CodeGraph.isInitialized(root)
    ? await CodeGraph.open(root)
    : await CodeGraph.init(root, { index: true });
  if (!graph.watch()) throw new Error('CodeGraph watcher did not start');
  await graph.waitUntilWatcherReady();
  // Catch changes made before the watch set was installed, including downtime.
  await graph.sync();
  send({ event: 'ready', freshness: status() });
}
let chain = start();
chain.catch(error => {
  console.error(error.stack || error.message);
  process.exit(1);
});
const input = readline.createInterface({ input: process.stdin });
input.on('close', close);
input.on('line', line => {
  if (closing) return;
  let request;
  try { request = JSON.parse(line); }
  catch (error) { console.error(error.message); return; }
  chain = chain.then(async () => {
    const { id, operation, query } = request;
    try {
      let result;
      switch (operation) {
        case 'status': result = graph.getStats(); break;
        case 'search': result = graph.searchNodes(query, { limit: 20 }); break;
        case 'context':
          result = await graph.buildContext(query, { maxNodes: 20, includeCode: true, format: 'markdown' });
          break;
        default: throw new Error('Unsupported operation: ' + operation);
      }
      send({ id, result, freshness: status() });
    } catch (error) {
      send({ id, error: error.message });
    }
  });
});
