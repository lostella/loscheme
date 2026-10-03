// Runs the interpreter off the main thread so the page stays responsive
// and a runaway program can be stopped by terminating this worker.
import init, { run_standard } from "./loscheme.js";

await init();
postMessage({ type: "ready" });

onmessage = ({ data }) => {
    const result = run_standard(data.code);
    postMessage({
        type: "result",
        output: result.output,
        value: result.value ?? null,
        error: result.error ?? null,
    });
    result.free();
};
