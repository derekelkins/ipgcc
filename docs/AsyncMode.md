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

The `at` method replaces array indexing. All generated functions will
become `async`, i.e. `Promise` returning, and will `await` on these
as needed. (In theory, I could exploit the parallelism in the data-dependency
graph to run `slices` in parallel Haxl-style, but I've not implemented that.)

The benefit of async mode is that only the parts of the input that are necessary
are fetched. For example, [docs/AsyncExample/viewer.html](AsyncExample/viewer.html)
uses `Range` headers to select the relevant subset of the input in `fetch`
requests. In practice, and as that example illustrates, you will want some kind of
caching/prefetching layer so you're not making a bunch of requests for individual
bytes.

(As of writing this, Firefox won't work right on that page unless you tell it
to only accept the `identity` encoding via `network.http.accept-encoding.secure`.
I'm not sure if it is Firefox or GitHub that is doing the wrong thing here. I'm
inclined to say the problem is with GitHub, though Firefox may also be doing something
wrong. Basically, Firefox *adds* `identity` to the list of encodings in `Accept-Encoding`,
but still has `gzip` (by default) in that list. GitHub's servers respond with
a range response, but also `Content-Encoding: gzip`. My guess is that the `Content-Encoding`
is completely spurious, since it's unclear what that would even mean for a range.
It seems Chrome and Edge just don't send `Accept-Encoding` [which implies only `identity`]
in this case forcing GitHub's hand.)
