# Decompress a DBC (compressed DBF) file

This function allows you decompress a DBC file into its DBF counterpart.
Please note that this is the file format used by the Brazilian Ministry
of Health (DATASUS), and it is not related to the FoxPro or CANdb DBC
file formats.

## Usage

``` r
dbc2dbf_wrapper(input.file, output.file)
```

## Source

The internal C code for `dbc2dbf` is based on `blast` decompressor and
`blast-dbf` (see *References*).

## Arguments

- input.file:

  The name of the DBC file (including extension)

- output.file:

  The output file name (including extension)

## Value

Return TRUE if succeeded, FALSE otherwise.

## Details

DBC is the extension for compressed DBF files (from the 'XBASE' family
of databases). This is a proprietary file format used by the brazilian
government to make available public healthcare datasets (by it's agency
called DATASUS).

It uses internally the PKWare's Data Compression Library (DCL) "implode"
compression algorithm. When decompressed, it becomes a regular DBF file.

## References

The PKWare ZIP file format documentation (contains the "implode"
algorithm specification) available at <https://pkware.my.site.com/s/>,
current version
<https://pkware.cachefly.net/webdocs/casestudies/APPNOTE.TXT>.

`blast` source code in C:
<https://github.com/madler/zlib/tree/master/contrib/blast>

`blast-dbf`, DBC to DBF command-line decompression tool:
<https://github.com/eaglebh/blast-dbf>

## See also

`read.dbc`

## Author

Daniela Petruzalek, <daniela.petruzalek@gmail.com>

## Examples

``` r
# \donttest{
in.f <- system.file("files/sids.dbc", package = "datazoom.saude")
out.f <- tempfile(fileext = ".dbf")
if (file.exists(in.f)) {
  dbc2dbf_wrapper(input.file = in.f, output.file = out.f)
}
# }
```
