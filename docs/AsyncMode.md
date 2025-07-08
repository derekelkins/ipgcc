Passing `--async-mode` as a flag when exporting JavaScript enables
a variant of the export using `async` functions. 

The input in this case needs to be an object matching the following
interface (or a suitably similar one):

```typescript
interface AsyncInput {
    slice(start: number, end: number): Promise<Uint8Array>;
    at(offset: number): Promise<number>;
}
```

The `at` method replaces array indexing. All generated methods will
become `async`, i.e. `Promise` returning, and will `await` on these
as needed. (In theory, I could exploit the parallelism in the data-dependency
graph to run `slices` in parallel Haxl-style, but I've not implemented that.)

The benefit of async mode is that fetching only the parts of the input
that are necessary. For example, [docs/AsyncExample/ISO9660.html](docs/AsyncExample/ISO9660.html)
uses `Range` headers to select the relevant subset of the input in `fetch`
requests. In practice, and as that example illustrates, you will want some kind of
caching/prefetching layer so you're not making a bunch of requests for individual
bytes.
