

# if the UTF-8 string can be represented in the native encoding, use native encoding
tryNativeEncoding <- function(string) {
  if (!isWindows()) return(string)
  string2 <- enc2native(string)
  if (identical(enc2utf8(string2), string)) string2 else string
}

isWindows <- function() .Platform$OS.type == 'windows'


# assume file is encoded in UTF-8, but warn against BOM
checkEncoding <- function(file) {
  # skip *nix because its locale is normally UTF-8 based (e.g. en_US.UTF-8), and
  # *nix users have to make a conscious effort to save a file with an encoding
  # that is not UTF-8; if they choose to do so, we cannot do much about it
  # except sitting back and seeing them punished after they choose to escape a
  # world of consistency (falling back to getOption('encoding') will not help
  # because native.enc is also normally UTF-8 based on *nix)
  if (!isWindows()) return('UTF-8')
  size <- file.info(file)[, 'size']
  if (is.na(size)) stop('Cannot access the file ', file)
  # BOM is 3 bytes, so if the file contains BOM, it must be at least 3 bytes
  if (size < 3L) return('UTF-8')
  
  # check if there is a BOM character: this is also skipped on *nix, because R
  # on *nix simply ignores this meaningless character if present, but it hurts
  # on Windows
  if (identical(charToRaw(readChar(file, 3L, TRUE)), charToRaw('\UFEFF'))) {
    warning('You should not include the Byte Order Mark (BOM) in ', file, '. ',
            'Please re-save it in UTF-8 without BOM. See ',
            'http://shiny.rstudio.com/articles/unicode.html for more info.')
    return('UTF-8-BOM')
  }
  'UTF-8'
}

# read a file using UTF-8 and (on Windows) convert to native encoding if possible
readUTF8 <- function(file) {
  enc <- checkEncoding(file)
  file <- base::file(file, encoding = enc)
  on.exit(close(file), add = TRUE)
  x <- enc2utf8(readLines(file, warn = FALSE))
  tryNativeEncoding(x)
}

sourceUTF8 <- function(file, envir = globalenv()) {
  lines <- readUTF8(file)
  enc <- if (any(Encoding(lines) == 'UTF-8')) 'UTF-8' else 'unknown'
  src <- srcfilecopy(file, lines, isFile = TRUE)  # source reference info
  # oddly, parse(file) does not work when file contains multibyte chars that
  # **can** be encoded natively on Windows (might be a bug in base R); we fall
  # back to parse(text) in this case
  exprs <- tryCatch(
    parse(file, keep.source = FALSE, srcfile = src, encoding = enc),
    error = function(e) {
      parse(text = lines, keep.source = FALSE, srcfile = src, encoding = enc)
    }
  )
  eval(exprs, envir)
}

sourceUTF8FromCode <- function(lines, envir = globalenv()) {
  enc <- if (any(Encoding(lines) == 'UTF-8')) 'UTF-8' else 'unknown'
  exprs <- parse(text = lines, keep.source = FALSE, encoding = enc)
  eval(exprs, envir)
}


