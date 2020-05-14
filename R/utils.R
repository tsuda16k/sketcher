

#' @importFrom stats runif median
#' @importFrom stringr str_match str_split str_sub
#' @importFrom graphics plot
#' @importFrom magrittr mod "%>%"
NULL


# CRAN sometimes issues spurious warnings about undefined variables
utils::globalVariables( c( ".", "%>%", "x", "y", "c", "value" ) )


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# nimg class ----


nimg = function( im, name ){
  if( is.logical( im ) | is.integer( im ) ){
    im = im + 0.0
  }
  if( length( dim( im ) ) == 2 ){ # gray-scale image
    dim( im ) = c( dim( im ), 1 )
  }
  class( im ) = c( "nimg", "numeric" )
  if( ! base::missing( name ) ){
    attr( im, "name" ) = name
  } else if( is.null( attr( im, "name" ) ) ){
    attr( im, "name" ) = ""
  }
  im
}


##' @export
print.nimg = function( x, ... ){
  d = dim( x )
  if( attr( x, "name" ) == "" || attr( x, "name" ) == "-" || is.null( attr( x, "name" ) ) ){
    name = "image"
  } else {
    name = attr( x, "name" )
  }
  cat( sprintf( "%s: %i [height] x %i [width] x %i [colour channels]\n", name, d[1], d[2], d[3] ) )
  # cat( sprintf( "image: %i [height] x %i [width] x %i [colour channels]\n", d[1], d[2], d[3] ) )
  invisible( x )
}


#' Display an image
#' @param x an image
#' @param rescale logical. if true, then pixel value is rescaled to range between 0 and 1.
#' @param ... other parameters to be passed to plot.default
#' @return No return value, called for side effects.
#' @examples
#' plot(face)
#' @export
plot.nimg = function( x, rescale = FALSE, ... ){
  old.par = graphics::par( no.readonly = TRUE )
  on.exit( graphics::par( old.par ), add = TRUE )
  if( im_npix( x ) == 0 ){
    stop( "The image is empty." )
  }

  if( im_nc( x ) == 1 ){
    # a raster array must have exactly 3 or 4 planes
    x = im_rep( x, 3 )
  }
  im = x[ ,,, drop = FALSE ]
  if( rescale ){
    im = rescaling01( im )
  } else if( max( im ) > 1 || min( im ) < 0 ){
    # warning( paste0( "Pixcel value exceeds the range [0,1], and hence it was clamped when plotting.\n",
    #                  "min = ", min( im ), ", max = ", max( im ) ) )
    im = clamping( im )
  }
  graphics::par( mar = c( 0, 0, 0, 0 ) )
  graphics::plot.new()
  graphics::plot.window(
    xlim = c(1,im_width(x)), ylim = c(im_height(x),1), asp = 1, xaxs = "i", yaxs = "i", ...)
  rst = grDevices::as.raster(
    im, rescale = FALSE, colorscale = NULL, colourscale = NULL, col.na = grDevices::rgb(0,0,0,0) )
  graphics::rasterImage( rst, 1, nrow( rst ), ncol( rst ), 1, interpolate = FALSE )
  invisible( x )
}


#' Load image from file or URL
#' @param file path to file or URL
#' @param name a string for name attribute. if missing, inferred from the file argument.
#' @return an array of image data
#' @examples
#' \dontrun{
#' # load an image from disk
#' im = im_load("path/to/your/image.jpg")
#' plot(im)
#' # load an image from URL
#' im = im_load("http://placehold.jp/150x150.png")
#' }
#' @export
im_load = function( file, name ){
  if( grepl("^(http|ftp)s?://", file) ){ # if URL
    url = file
    ext = stringr::str_extract_all( url, "\\.([A-Za-z0-9]+$)" )[[ 1 ]]
    if( length( ext ) > 0 ){
      file = tempfile( fileext = ext )
    } else {
      file = tempfile()
    }
    downloader::download( url, file, mode = "wb" )
    im = im_load( file, get_image_name_from_file( url ) )
    unlink( file )
    return( im )
  }
  ext = sub( ".*\\.([^.]{3,4})$", "\\1", file ) %>% tolower
  if( ext %in% c( "png", "bmp", "jpg", "jpeg" ) ){
    tryCatch({
      im = readbitmap::read.bitmap( file )
    },
    error = function(e) {
      stop( paste0( e, "Note: im_load() fails for binary (black/white) bmp image." ) )
    })
    # im = readbitmap::read.bitmap( file )
    dim( im )
    if( ! is.null( attr( im, "header" ) ) ){
      im = im / 255
    }
    if( length( dim( im ) ) == 2 ){ # gray-scale image
      dim( im ) = c( dim( im ), 1 )
    } else if( length( dim( im ) ) == 3 ){ # multiple channels
      if( dim( im )[ 3 ] %in% c( 2, 4 ) ){
        # remove alpha channel if it is uninformative
        if( min( im[ , , dim( im )[ 3 ] ] ) == max( im[ , , dim( im )[ 3 ] ] ) ){
          im = im[ , , 1:( dim( im )[ 3 ] - 1 ), drop = FALSE ]
        }
      }
    }
    im = nimg( im, ifelse( base::missing( name ), get_image_name_from_file( file ), name ) )
    return( im )
  } else {
    stop( "Only jpg, png, and bmp formats are supported." )
  }
}


get_image_name_from_file = function( file ){
  tryCatch({
    name = stringr::str_split( file, "/" )[[ 1 ]]
    name = name[ length( name ) ]
    name = stringr::str_split( name, "[.]" )[[ 1 ]]
    return( name[ 1 ] )
  },
  error = function(e) {
    return( "-" )
  })
}


#' Save an image to disk
#' @param im An image.
#' @param name Name of the image file.
#' @param path Path to file.
#' @param format Image format. Either "jpg", "png", "tiff", or "bmp". Default is "png".
#' @param quality (jpg only) default is 0.95. Higher quality means less compression.
#' @return No return value, called for side effects.
#' @examples
#' \dontrun{
#' im = sketch(face)
#'
#' # im.png is saved to the current working directory
#' im_save( im, name = "im", path = getwd() )
#'
#' # myimage.jpg is saved to a specified directory
#' im_save( im, name = "myimage", path = "path/to/image", format = "jpg" )
#' }
#' @export
im_save = function( im, name, path, format = "png", quality = .95 ){
  if( ! format %in% c( "jpg", "png" ) ){
    warning( "Incorrect imaeg format. Use either jpg or png." )
    return()
  }
  if( base::missing( name ) ){
    name = deparse( substitute( im ) )
  }
  if( im_nc( im ) == 1 ){
    im = im_rep( im, 3 )
  }
  if( stringr::str_sub( path, stringr::str_length( path ) ) == "/" ){
    path = stringr::str_sub( path, end = stringr::str_length( path ) - 1 )
  }
  if( max( im ) > 1 || min( im ) < 0 ){
    # warning( "Pixcel value exceeds the range [0,1], and hence it was clamped when saving.")
    im = clamping( im )
  }
  base::dir.create( path, showWarnings = FALSE, recursive = TRUE )
  file = paste0( path, "/", name, ".", format )
  if( format == "png" ){
    png::writePNG( im, file )
  } else if ( format == "jpeg" | format == "jpg" ){
    jpeg::writeJPEG( im, file, quality = quality )
  }
}


cimg2nimg = function( im ){
  if( is.list( im ) ){
    im = lapply( im, function( x ){
      if( "nimg" %in% class( x ) ){
        x
      } else {
        cimg2nimg( x )
      }
    })
    return( im )
  } else if( any( c( "cimg", "pixset" ) %in% class( im ) ) ){
    im = aperm( im, c( 2, 1, 4, 3 ) ) # (x, y, z, cc) to (y, x, cc, z)
    return( nimg( im[,,,1] ) )
  } else if( "nimg" %in% class( im ) ){
    return( im )
  } else {
    return( nimg( im ) )
  }
}


nimg2cimg = function( im ){
  if( is.list( im ) ){
    im = lapply( im, function(x){
      if( any( c( "cimg", "pixset" ) %in% class( x ) ) ){
        x
      } else {
        nimg2cimg( x )
      }
    })
    return( im )
  } else {
    if( any( c( "cimg", "pixset" ) %in% class( im ) ) ) {
      return( im )
    } else if( length( dim( im ) ) == 2 ){ # (y, x) to (x, y)
      return( imager::as.cimg( t( im ) ) )
    } else if( length( dim( im ) ) == 4 ){ # (y, x, cc, z) to (x, y, z, cc)
      return( imager::as.cimg( aperm( im, c( 2, 1, 4, 3 ) ) ) )
    } else if( length( dim( im ) ) == 3 ){ # (y, x, cc) to (x, y, cc)
      im = aperm( im, c( 2, 1, 3 ) )
      im2 = array( 0, dim = c( dim( im )[ 1 ], dim( im )[ 2 ], 1, dim( im )[ 3 ] ) )
      im2[,,1,] = im
      return( imager::as.cimg( im2 ) )
    }
  }
}


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# color space ----


sRGB2RGB = function( im ){
  mask = im < 0.04045
  im[ mask ] = im[ mask ] / 12.92
  im[ !mask ] = ( ( im[ !mask ] + 0.055 ) / 1.055 )^2.4
  return( im )
}


RGB2sRGB = function( im ){
  mask = im < 0.0031308
  im[ mask ] = im[ mask ] * 12.92
  im[ !mask ] = 1.055 * im[ !mask ]^( 1 / 2.4 ) - 0.055
  return( im )
}


RGB2XYZ = function( im, use.D65 = TRUE ){
  if( use.D65 ){
    X = 0.4124564 * get_R( im ) + 0.3575761 * get_G( im ) + 0.1804375 * get_B( im )
    Y = 0.2126729 * get_R( im ) + 0.7151522 * get_G( im ) + 0.0721750 * get_B( im )
    Z = 0.0193339 * get_R( im ) + 0.1191920 * get_G( im ) + 0.9503041 * get_B( im )
  } else {
    X = 0.4360747 * get_R( im ) + 0.3850649 * get_G( im ) + 0.1430804 * get_B( im )
    Y = 0.2225045 * get_R( im ) + 0.7168786 * get_G( im ) + 0.0606169 * get_B( im )
    Z = 0.0139322 * get_R( im ) + 0.0971045 * get_G( im ) + 0.7141733 * get_B( im )
  }
  return( merge_color( list( X, Y, Z ) ) )
}


XYZ2RGB = function( im, use.D65 = TRUE ){
  if( use.D65 ){
    R =  3.24045484 * get_R( im ) - 1.5371389 * get_G( im ) - 0.49853155 * get_B( im )
    G = -0.96926639 * get_R( im ) + 1.8760109 * get_G( im ) + 0.04155608 * get_B( im )
    B =  0.05564342 * get_R( im ) - 0.2040259 * get_G( im ) + 1.05722516 * get_B( im )
  } else {
    R =  3.13385637 * get_R( im ) - 1.6168668 * get_G( im ) - 0.49061477 * get_B( im )
    G = -0.97876856 * get_R( im ) + 1.9161416 * get_G( im ) + 0.03345412 * get_B( im )
    B =  0.07194517 * get_R( im ) - 0.2289913 * get_G( im ) + 1.40524267 * get_B( im )
  }
  return( merge_color( list( R, G, B ) ) )
}


sRGB2XYZ = function( im, use.D65 = TRUE ){
  im %>% sRGB2RGB %>% RGB2XYZ( use.D65 )
}


XYZ2sRGB = function( im, use.D65 = TRUE ){
  im %>% XYZ2RGB( use.D65 ) %>% RGB2sRGB
}


XYZ2Lab = function( im, use.D65 = TRUE ){
  # reference white
  if( use.D65 ){
    white = c( 0.95047, 1, 1.08883 )
  } else {
    white = c( 0.96420, 1, 0.82491 )
  }
  im[ ,,1 ] = im[ ,,1, drop = FALSE ] / white[ 1 ]
  im[ ,,3 ] = im[ ,,3, drop = FALSE ] / white[ 3 ]
  #
  mask = 24389 * im > 216
  im[ mask ] = im[ mask ]^( 1 / 3 )
  im[ !mask ] = ( 24389 * im[ !mask ] / 27 + 16 ) / 116
  fx = im[ ,,1, drop = FALSE ]
  fy = im[ ,,2, drop = FALSE ]
  fz = im[ ,,3, drop = FALSE ]
  #
  L = ( 116 * fy - 16 )
  a = 500 * ( fx - fy )
  b = 200 * ( fy - fz )
  return( merge_color( list( L, a, b ) ) )
}


Lab2XYZ = function( im, use.D65 = TRUE ){
  eta = 216 / 24389
  kappa = 24389 / 27
  #
  fy = ( im[,,1, drop = FALSE ] + 16 ) / 116
  fx = 0.002 * im[,,2, drop = FALSE ] + fy
  fz = fy - 0.005 * im[,,3, drop = FALSE ]
  # x = fx^3 > eta ? fx^3 : ( 116 * fx - 16 ) / kappa
  mask = fx^3 > eta
  fx[ mask ] = fx[ mask ]^3
  fx[ !mask ] = ( 116 * fx[ !mask ] - 16 ) / kappa
  # y = L > 8 ? ( ( L + 16 ) / 116 )^3 : L / kappa
  L = im[,,1, drop = FALSE ]
  mask = L > 8
  L[ mask ] = ( ( L[ mask ] + 16 ) / 116 )^3
  L[ !mask ] = L[ !mask ] / kappa
  # z = fz^3 > eta ? fz^3 : ( 116 * fz - 16 ) / kappa
  mask = fz^3 > eta
  fz[ mask ] = fz[ mask ]^3
  fz[ !mask ] = ( 116 * fz[ !mask ] - 16 ) / kappa
  # reference white
  if( use.D65 ){
    white = c( 0.95047, 1, 1.08883 )
  } else {
    white = c( 0.96420, 1, 0.82491 )
  }
  fx = fx * white[ 1 ]
  fz = fz * white[ 3 ]
  return( merge_color( list( fx, L, fz ) ) )
}


sRGB2Lab = function( im, use.D65 = TRUE ){
  XYZ2Lab( sRGB2XYZ( im, use.D65 ), use.D65 )
}


Lab2sRGB = function( im, use.D65 = TRUE ){
  XYZ2sRGB( Lab2XYZ( im, use.D65 ), use.D65 )
}


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# math ----


rescaling01 = function( x ){
  if( max( x ) == min( x ) ){
    return( x )
  } else {
    return( ( x - min( x ) ) / ( max( x ) - min( x ) ) )
  }
}


clamping = function( x, min = 0, max = 1 ){
  x[ x < min ] = min
  x[ x > max ] = max
  return( x )
}


cubic_spline = function( x, low = 0, high = 1 ){
  if( low == high ){
    warning( "low and high must be different!" )
  } else if( low > high ){
    return( 1 - ( cubic_spline( x, high, low ) ) )
  }
  x2 = x
  t = x[ x > low & x < high ]
  t = ( t - low ) / ( high - low )
  x2[ x > low & x < high ] = t^2 * ( 3 - 2 * t )
  x2[ x <= low ] = 0
  x2[ x >= high ] = 1
  return( x2 )
}


ramp_threshold = function( x, eta, phi ){
  y = x
  y[ x >= eta ] = 1
  y[ x < eta ] = 1 + tanh( phi * ( y[ x < eta ] - eta ) )
  return( y )
}


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# image info ----


im_height = function( im ){
  dim( im )[ 1 ]
}


im_width = function( im ){
  dim( im )[ 2 ]
}


im_size = function( im ){
  unname( dim( im )[ 1:2 ] )
}


im_npix = function( im ){
  prod( dim( im ) )
}


im_nc = function( im ){
  dim( im )[ 3 ]
}


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# image slicing ----


force_channel_label_to_num = function( x ){
  if( is.numeric( x ) ){
    return( x )
  }
  y = c()
  for( i in 1:length( x ) ){
    if( x[ i ] %in% c( "R", "r", "L", "l" ) ){
      y = c( y, 1 )
    } else if( x[ i ] %in% c( "G", "g", "a" ) ){
      y = c( y, 2 )
    } else if( x[ i ] %in% c( "B", "b" ) ){
      y = c( y, 3 )
    } else if( x[ i ] %in% c( "A", "alpha", "Alpha" ) ){
      y = c( y, 4 )
    } else {
      y = c( y, 0 )
    }
  }
  return( y )
}


get_channel = function( im, channel ){
  if( length( dim( im ) ) == 2 ){
    return( im )
  } else {
    return( nimg( im[ , , force_channel_label_to_num( channel ), drop = FALSE ] ) )
  }
}


get_R = function( im ){
  return( get_channel( im, 1 ) )
}


get_G = function( im ){
  return( get_channel( im, 2 ) )
}


get_B = function( im ){
  return( get_channel( im, 3 ) )
}


split_color = function( im ){
  ls = list()
  for( i in 1:dim( im )[ 3 ] ){
    ls = c( ls, list( nimg( im[ , , i, drop = FALSE ] ) ) )
  }
  return( ls )
}


merge_color = function( imlist ){
  imdim = dim( imlist[[ 1 ]] )
  im = array( 0, c( imdim[ 1 ], imdim[ 2 ], length( imlist ) ) )
  for( i in 1:length( imlist ) ){
    im[,,i] = imlist[[ i ]]
  }
  return( nimg( im ) )
}


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# image transform ----


im_rep = function( im, n = 3, channel = 1 ){
  nimg( array( get_channel( im, channel ), c( im_height( im ), im_width( im ), n ) ) )
}


im_pad = function( im, n, method = "mirror" ){
  if( n == 0 ) return( im )

  w = im_width( im )
  h = im_height( im )

  if( any( n > c( w, h ) ) ){
    warning( "n must be equal or smaller than image width (and height)." )
    return( im )
  }

  # create an empty matrix
  x = ifelse( is.numeric( method ), method, ifelse( method == "mean", mean( im ), 0 ) )
  mat = array( x, c( h + 2 * n, w + 2 * n, dim( im )[ 3 ] ) )

  # put the image
  mat[ ( n + 1 ):( n + h ), ( n + 1 ):( n + w ), ] = im

  # padding
  if( method == "zero" || method == "mean" || is.numeric( method ) ){
    # do nothing
  } else if( method == "repeat" ){
    # top left
    mat[ 1:n, 1:n, ] = im[ (h-n+1):h, (w-n+1):w, ]
    # top
    mat[ 1:n, (n+1):(n+w), ] = im[ (h-n+1):h, 1:w, ]
    # top right
    mat[ 1:n, (n+w+1):(2*n+w), ] = im[ (h-n+1):h, 1:n, ]
    # left
    mat[ (n+1):(n+h), 1:n, ] = im[ 1:h, (w-n+1):w, ]
    # right
    mat[ (n+1):(n+h), (n+w+1):(2*n+w), ] = im[ 1:h, 1:n, ]
    # bottom left
    mat[ (n+h+1):(2*n+h), 1:n, ] = im[ 1:n, (w-n+1):w, ]
    # bottom
    mat[ (n+h+1):(2*n+h), (n+1):(n+w), ] = im[ 1:n, 1:w, ]
    # bottom right
    mat[ (n+h+1):(2*n+h), (n+w+1):(2*n+w), ] = im[ 1:n, 1:n, ]
  } else if( method == "mirror" ){
    # top left
    mat[ 1:n, 1:n, ] = im[ n:1, n:1, ]
    # top
    mat[ 1:n, (n+1):(n+w), ] = im[ n:1, 1:w, ]
    # top right
    mat[ 1:n, (n+w+1):(2*n+w), ] = im[ n:1, w:(w-n+1), ]
    # left
    mat[ (n+1):(n+h), 1:n, ] = im[ 1:h, n:1, ]
    # right
    mat[ (n+1):(n+h), (n+w+1):(2*n+w), ] = im[ 1:h, w:(w-n+1), ]
    # bottom left
    mat[ (n+h+1):(2*n+h), 1:n, ] = im[ h:(h-n+1), n:1, ]
    # bottom
    mat[ (n+h+1):(2*n+h), (n+1):(n+w), ] = im[ h:(h-n+1), 1:w, ]
    # bottom right
    mat[ (n+h+1):(2*n+h), (n+w+1):(2*n+w), ] = im[ h:(h-n+1), w:(w-n+1), ]
  }

  im = nimg( mat )
  return( im )
}


im_crop = function( im, margin ){
  if( length( margin ) == 1 ){
    top = bottom = left = right = margin
  } else if( length( margin ) == 2 ){
    top = bottom = margin[ 1 ]
    left = right = margin[ 2 ]
  } else if( length( margin ) == 3 ){
    warning( "margin length must be 1, 2, or 4!" )
  } else if( length( margin ) == 4 ){
    top = margin[ 1 ]
    right = margin[ 2 ]
    bottom = margin[ 3 ]
    left = margin[ 4 ]
  }
  im = im[ (1 + top):(im_height( im ) - bottom), (1 + left):(im_width( im ) - right), , drop = FALSE ]
  return( nimg( im ) )
}


im_crop_square = function( im, position = 0.5 ){
  position = clamping( position )
  diff = im_width( im ) - im_height( im )
  position = 2 * position - 1 # range [-1,1]
  size = min( im_size( im ) )
  erode = abs( diff ) / 2
  center = max( im_size( im ) ) / 2
  start = floor( center - size / 2 + erode * position )
  if( start < 1 ) start = 1
  end = start + size - 1
  if( diff > 0 ){ # wide
    im = im_crop( im, c( 0, im_width( im ) - end, 0, start - 1 ) )
  } else { # tall
    im = im_crop( im, c( start - 1, 0, im_height( im ) - end, 0 ) )
  }
  return( nimg( im ) )
}


im_resize = function( im, height, width, interpolation = 1 ){
  itype = 1 + 2 * interpolation # 0->1, 1->3, 2->5
  if( base::missing( width ) ){ # scale to height
    width = round( im_width( im ) * ( height / im_height( im ) ) )
  } else if( base::missing( height ) ){ # scale to width
    height = round( im_height( im ) * ( width / im_width( im ) ) )
  }
  im = imager::resize( nimg2cimg( im ), size_x = width, size_y = height, interpolation_type = itype )
  return( cimg2nimg( im ) )
}


im_resize_limit = function( im, bound, interpolation = 1 ){
  if( max( im_size( im ) ) < bound ){
    return( im )
  }
  if( im_width( im ) > im_height( im ) ){
    im_resize( im, width = bound, interpolation = interpolation )
  } else {
    im_resize( im, height = bound, interpolation = interpolation )
  }
}


im_resize_scale = function( im, scale = 1, interpolation = 1 ){
  itype = 1 + 2 * interpolation # 0->1, 1->3, 2->5
  im = imager::imresize( nimg2cimg( im ), scale, itype )
  return( cimg2nimg( im ) )
}


im_combine = function( im1, im2, y = 0, x = 0, alpha = FALSE, background = 1 ){
  cc = max( im_nc( im1 ), im_nc( im2 ) )
  h = max( im_height( im1 ), y + im_height( im2 ), im_height( im2 ), - y + im_height( im1 ) )
  w = max( im_width( im1 ), x + im_width( im2 ), im_width( im2 ), - x + im_width( im1 ) )
  im = array( rep( background, each = h * w, times = cc ), dim = c( h, w, cc ) )

  y1 = ifelse( y < 0, -y, 0 ) + 1
  y2 = ifelse( y < 0, 0, y ) + 1
  x1 = ifelse( x < 0, -x, 0 ) + 1
  x2 = ifelse( x < 0, 0, x ) + 1
  im[ y1:( y1 + im_height( im1 ) - 1 ), x1:( x1 + im_width( im1 ) - 1 ), 1:cc ] = im1
  im[ y2:( y2 + im_height( im2 ) - 1 ), x2:( x2 + im_width( im2 ) - 1 ), 1:cc ] = im2
  if( ! alpha ){
    return( nimg( im ) )
  } else {
    A = array( 0, dim = c( h, w, 1 ) )
    A[ y1:( y1 + im_height( im1 ) - 1 ), x1:( x1 + im_width( im1 ) - 1 ), 1 ] = 1
    A[ y2:( y2 + im_height( im2 ) - 1 ), x2:( x2 + im_width( im2 ) - 1 ), 1 ] = 1
    return( merge_color( c( split_color( im ), list( A ) ) ) )
  }
}


im_raise = function( im, intercept ){
  intercept + ( 1 - intercept ) * im
}


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# luminance ----


im_gray = function( im, tricolored = FALSE ){
  if( im_nc( im ) < 2 ){
    return( im )
  }
  lab = sRGB2Lab( im )
  L = get_R( lab )
  C0 = array( 0, dim = dim( L ) )
  im = merge_color( list( L, C0, C0 ) ) %>% Lab2sRGB
  if( ! tricolored ){
    im = get_R( im )
  }
  return( im )
}


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# spatial filtering ----


box_blur = function( im, radius ){
  if( radius < 1 ){
    warning( "radius should be equal to or larger than 1.")
    return( im )
  }
  r = radius
  if( im_nc( im ) != 1 ){
    imlist = list()
    for( i in 1:im_nc( im ) ){
      imlist = c( imlist, list( box_blur( get_channel( im, i ), r ) ) )
    }
    return( merge_color( imlist ) )
  }
  L = 2 * r + 1
  width = im_width( im )
  height = im_height( im )
  im = im_pad( im, r, method = "mirror" )
  out = array( 0.0, dim( im ) )
  cumsum = rowSums( im[ , 1:(2*r), ] )
  # i = r + 1
  cumsum = cumsum + im[ ,r + 1 + r, ]
  out[ , r + 1, ] = cumsum / L
  for( i in ( r + 2 ):( width + r ) ){
    cumsum = cumsum + im[ ,i + r, ] - im[ ,i - r - 1, ]
    out[ , i, ] = cumsum / L
  }
  im = out
  cumsum = colSums( im[ 1:(2*r), , ] )
  cumsum = cumsum + im[ r + 1 + r, , ]
  out[ r + 1, , ] = cumsum / L
  for( i in ( r + 2 ):( height + r ) ){
    cumsum = cumsum + im[ i + r, , ] - im[ i - r - 1, , ]
    out[ i, , ] = cumsum / L
  }
  out = im_crop( out, r )
  return( out )
}


box_variance = function( im, radius ){
  box_blur( im^2, radius ) - box_blur( im, radius )^2
}


gauss_kernel = function( sd, radius = round( 2.5 * sd ) ){
  if( sd < 0.2 ){
    warning( "sd must be equal to or larger than 0.2")
    return( NULL )
  }
  L = 2 * radius + 1
  matx = matrix( stats::dnorm( 1:L, mean = radius + 1, sd = sd ), nrow = L, ncol = L, byrow = FALSE )
  maty = matrix( stats::dnorm( 1:L, mean = radius + 1, sd = sd ), nrow = L, ncol = L, byrow = TRUE )
  mat = matx * maty
  mat = mat / sum( mat )
  return( nimg( array( mat, c( L, L, 1 ) ) ) )
}


guided_filter = function( p, radius, epsilon = 0.1, I = p ){
  if( radius < 1 ){
    warning( "radius should be equal to or larger than 1.")
    return( p )
  }

  I_mean = box_blur( I, radius )
  I_var = box_variance( I, radius )
  p_mean = box_blur( p, radius )

  a = ( box_blur( I * p, radius ) - I_mean * p_mean ) / ( I_var + epsilon )
  b = p_mean - a * I_mean

  a_mean = box_blur( a, radius )
  b_mean = box_blur( b, radius )

  q = a_mean * I + b_mean
  return( q )
}


stat_filter = function( im, radius, FUN, pad.method = "mirror" ){
  if( radius < 1 ){
    warning( "radius should be equal to or larger than 1.")
    return( im )
  }

  if( im_nc( im ) > 1 ){
    imlist = list()
    for( i in 1:im_nc( im ) ){
      imlist = c( imlist, list( stat_filter( get_channel( im, i ), radius, FUN, pad.method ) ) )
    }
    return( merge_color( imlist ) )
  }

  im = im_pad( im, radius, method = pad.method )[,,]
  im2 = im
  for( cy in ( 1 + radius ):( im_height( im ) - radius ) ){
    for( cx in ( 1 + radius ):( im_width( im ) - radius ) ){
      im2[ cy, cx ] = FUN(
        as.vector( im[ ( cy - radius ):( cy + radius ), ( cx - radius ):( cx + radius ) ] )
      )
    }
  }
  im2 = im_crop( nimg( im2 ), radius )
  return( im2 )
}


DOG = function( im, sigma, k = 1.6 ){
  im_conv( im, gauss_kernel( sigma ) ) - im_conv( im, gauss_kernel( k * sigma ) )
}


XDOG = function( im, sigma, k = 1.6, p = 20 ){
  ( 1 + p ) * im_conv( im, gauss_kernel(sigma) ) - p * im_conv( im, gauss_kernel(k * sigma) )
}


im_conv = function( im, kernel, pad.method = "mirror" ){
  if( is.null( kernel ) ){
    return( im )
  }
  if( im_nc( im ) > 1 ){
    imlist = list()
    for( i in 1:im_nc( im ) ){
      imlist = c( imlist, list( im_conv( get_channel( im, i ), kernel, pad.method ) ) )
    }
    return( merge_color( imlist ) )
  }
  npad = floor( max( dim( kernel )[ 1:2 ] ) / 2 )
  im = im_pad( im, n = npad, method = pad.method )
  im = imager::convolve( nimg2cimg( im ), nimg2cimg( kernel ) )
  im = imager::crop.borders( im, nPix = npad )
  return( cimg2nimg( im ) )
}


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# material editing ----


gf_decompose = function( im, log_epsilon = 0.0001, filter_epsilon = 0.01 ){
  if( im_nc( im ) == 2 || im_nc( im ) > 3 ){
    warning( "The number of color channel must be either 1 or 3.")
    return( NULL )
  }
  if( im_nc( im ) == 3 ){
    lab = sRGB2Lab( im )
    dec = gf_decompose( get_channel( lab, 1 ) / 100 )
    dec = c( dec, list( a = get_channel( lab, 2 ), b = get_channel( lab, 3 ) ) )
    dec$n.color = 3
    return( dec )
  }

  dec = gf_decompose_scale( im, log_epsilon, filter_epsilon )
  dec = gf_decompose_parts( dec )

  return( dec )
}


gf_decompose_scale = function( im, depth = NULL, log_epsilon = 0.0001, filter_epsilon = 0.01 ){
  im = im_gray( im )
  if( is.null( depth ) ){
    depth = floor( log2( min( im_size( L ) ) ) )
  }
  L = log( im + log_epsilon )

  if( depth == 0 ) {
    N = 0
    D = list( residual = L )
  } else {
    # L0 = L
    # Lk = guided_filter( Lk-1, filter_epsilon, 2^k ) (k=1~n)
    # Dk = Lk-1 - Lk
    # recon = ∑(Dk)[k=1~n] + Ln
    N = min( depth, floor( log2( min( im_size( L ) ) ) ) )
    L_k_minus_1 = guided_filter( L, 2^1, filter_epsilon ) # L1
    D_k = L - L_k_minus_1 # D1
    D = list( D_k )
    if( N > 1 ){
      for( k in 2:N ){
        L_k = guided_filter( L_k_minus_1, 2^k, filter_epsilon )
        D_k = L_k_minus_1 - L_k
        D = c( D, list( D_k ) )
        if( k == N ){
          names( D ) = paste0( "D", sprintf( paste0( "%0", nchar( N ), "d" ), 1:N ) )
          # add residual
          D = c( D, list( residual = L_k ) )
        } else {
          L_k_minus_1 = L_k
        }
      }
    } else if( N == 1 ) {
      names( D ) = paste0( "D", sprintf( paste0( "%0", nchar( N ), "d" ), 1:N ) )
      D = c( D, list( residual = L_k_minus_1 ) )
    }
  }

  dec = list(
    size = im_size( im ),
    depth = N,
    n.color = 1,
    log_epsilon = log_epsilon,
    filter_epsilon = filter_epsilon,
    L = D
  )
  return( dec )
}


gf_decompose_parts = function( dec ){
  L = dec$L
  residual = L$residual
  L$residual = NULL
  L = lapply( L, function( im ){
    blur_range = 0.2
    range_lo = 1 - blur_range
    range_hi = 1 + blur_range
    sigma = stats::sd( im )
    hi =
      im * cubic_spline( im, range_lo * sigma, range_hi * sigma ) +
      im * cubic_spline( im, -range_lo * sigma, -range_hi * sigma )
    lo =
      im * pmin( cubic_spline( im, -range_hi * sigma, -range_lo * sigma ),
                 cubic_spline( im, range_hi * sigma, range_lo * sigma ) )
    hip = hi
    hip[ hi < 0 ] = 0
    hin = hi
    hin[ hi > 0 ] = 0
    lop = lo
    lop[ lo < 0 ] = 0
    lon = lo
    lon[ lo > 0 ] = 0
    return( list( highamp_posi = hip, highamp_nega = hin, lowamp_posi = lop, lowamp_nega = lon ) )
  } )
  L = c( L, list( residual = residual ) )
  dec$L = L
  return( dec )
}


gf_reconstruct = function( dec, scales, ind, include.residual = TRUE ){
  if( base::missing( scales ) ){
    scales = 1:dec$depth
  }
  if( base::missing( ind ) ){
    ind = 1:4
  }

  recon = array( 0, c( dec$size, 1 ) )
  if( ! any( 0 == scales ) && length( dec$L ) > 1 ){
    for( i in scales ){
      if( "nimg" %in% class( dec$L[[ i ]] ) ){
        # scale-only decomposition
        recon = recon + dec$L[[ i ]]
      } else {
        # scale and parts decomposition
        for( j in ind ){
          recon = recon + dec$L[[ i ]][[ j ]]
        }
      }
    }
  }
  if( include.residual ){
    recon = recon + dec$L$residual
  }
  recon = exp( recon ) - dec$log_epsilon

  if( dec$n.color == 3 ){
    recon = Lab2sRGB( merge_color( list( recon * 100, dec$a, dec$b ) ) )
  }
  return( recon )
}


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# sketcher ----


#' Apply the sketch effect on an image
#' @param im an image (array).
#' @param style a numeric (integer). Either 1 or 2.
#' @param lineweight a numeric. Strength of lines.
#' @param smooth a numeric (integer). Smoothness of image texture.
#' @param gain a numeric between 0 and 1. Can be used to reduce noise in dim regions.
#' @param contrast a numeric (integer). Adjusts the image contrast.
#' @param shadow a numeric between 0 and 1
#' @param max.size maximum image resolution (width or height) of the output image
#' @return an image.
#' @examples
#' im = sketch(face)
#' plot(im)
#'
#' \dontrun{
#' im = im_load("path/to/your/image.jpg")
#' plot(im)
#' }
#' @export
sketch = function( im, style = 1, lineweight = 1, smooth = lineweight, gain = .02, contrast = NULL,
                   shadow = 0, max.size = 2048 ){
  smooth = round( max( smooth, 1 ) )
  if( is.null( contrast ) ){
    contrast = ifelse( style == 1, 20, 4 )
  }

  im = im %>% im_resize_limit( max.size ) %>% im_gray()

  if( shadow ){
    shadow.smooth = -10 * shadow + 11
    tone = sketch_XDOG( im, smooth, sigma = 0.5, k = 1.6, p = 20, eta = shadow, phi = shadow.smooth )
  }

  im = im_raise( im, gain )
  N = floor( log2( min( im_size( im ) ) ) )
  if( smooth > N ){
    warning( paste0( "smooth exceeded the maximum possible value for the input image. smooth = ",
                     N, " was used instead.") )
    smooth = N
  }

  if( smooth >= 1 ){
    im = im %>% gf_decompose_scale( smooth ) %>% gf_reconstruct( scales = 0 )
  }

  if( style == 1 ){
    temp = im_conv( im, gauss_kernel( max( 0.3, lineweight ) ), pad.method = "mirror" )
  } else if( style == 2 ){
    temp = stat_filter( im, max( 1, lineweight ), max, pad.method = "mirror" )
  }

  im2 = clamping( im / temp )
  im2 = im2^contrast
  if( shadow ){
    im2 = pmin( im2, ( 1 - ( 1 - tone ) * 0.87 ) )
  }
  return( im2 )
}


sketch_XDOG = function( im, smooth = 1, sigma = 0.5, k = 1.6, p = 20, eta = 0.5, phi = 6 ){
  im %>% gf_decompose_scale( smooth ) %>% gf_reconstruct( scales = 0 ) %>%
    XDOG( sigma, k, p ) %>% ramp_threshold( eta, phi ) %>% clamping
}


#' Create multiple sketches at once and combine them into a single image
#'
#' It is often necessary to find optimal sketch style parameters for your task.
#' With this function, you can easily compare the effects of different style parameters.
#'
#' @param im an image.
#' @param style numeric (integer). Either 1 (edge-focused) or 2 (smooth gradient)
#' @param weight_levels numeric (integer). a vector of lineweight values
#' @param smooth_levels numeric (integer). a vector of smooth values
#' @param gain a numeric between 0 and 1. Can be used to reduce noise in dim regions.
#' @param contrast numeric (integer). Adjusts the image contrast.
#' @param black a numeric between 0 and 1
#' @param verbose If TRUE (default), progress information is displayed in the Console.
#' @return an array of the sketched image.
#' @export
#' @examples
#' \donttest{
#' im = survey(face, style = 1, weight_levels = c(1, 3), smooth_levels = c(1, 3))
#' plot(im)
#'
#' im = survey(face, style = 1, weight_levels = c(1, 3), smooth_levels = c(1, 3), black = TRUE)
#' plot(im)
#' }
survey = function( im, style = 1, weight_levels = c(1, 2, 4), smooth_levels = c(1, 2, 4),
                   gain = .02, contrast = NULL, black = FALSE, verbose = TRUE ){
  if( is.null( contrast ) ){
    contrast = ifelse( style == 1, 20, 4 )
  }
  if( verbose ){
    N = length( smooth_levels ) * length( weight_levels )
    n = 1
    cat( paste0( "Sketching ", N, " images: ") )
  }

  imgs = NULL
  for( s in 1:length( smooth_levels ) ){
    for( t in 1:length( weight_levels ) ){
      if( verbose ){
        cat( paste0( n, " ") )
      }
      im2 = sketch( im, style, weight_levels[ t ], smooth_levels[ s ], gain, contrast, black )
      if( is.null( imgs ) ){
        imgs = im2
      } else {
        y = ( s - 1 ) * im_height( im )
        x = ( t - 1 ) * im_width( im )
        imgs = im_combine( imgs, im2, y, x )
      }
      if( verbose ){
        n = n + 1
      }
    }
  }
  if( verbose ){
    cat( "done.\n" )
  }

  return( imgs )
}



