

clamping = function( x, min = 0, max = 1 ){
  x[ x < min ] = min
  x[ x > max ] = max
  return( x )
}



raise_bottom = function( x, intercept ){
  y = intercept + ( 1 - intercept ) * x
  return( y )
}



cimg2array = function( im ){
  # (x, y, z, cc) to (y, x, cc, z)
  return( base::aperm( im, c( 2, 1, 4, 3 ) ) )
}



array2cimg = function( im ){
  if( "cimg" %in% class( im ) ) {
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



im_load_cimg = function( path ){
  im = imager::load.image( path )
  if( imager::spectrum( im ) == 3 ){
    return( im )
  } else if( imager::spectrum( im ) < 3 ){
    return( imager::R( im ) )
  } else if( imager::spectrum( im ) > 3 ){
    return( imager::imappend(
      list( imager::R( im ), imager::G( im ), imager::B( im ) ), axis = "c" ) )
  }
}



#' Load image from file
#'
#' The png, jpg, and bmp are supported.
#'
#' @param path path to an image file
#' @return an array representing the image
#' @importFrom magrittr %>%
#' @export
#' @examples
#' \dontrun{
#' im = im_load("path/to/your/image.jpg")
#' pplot(im)
#' }
im_load = function( path ){
  im_load_cimg( path ) %>%
    cimg2array %>%
    return
}



#' Save an image to disk
#'
#' @param im An image.
#' @param name Name of the image file.
#' @param path Path to file.
#' @param format Image format. Either "jpg", "png", "tiff", or "bmp". Default is "png".
#' @param quality (jpg only) default is 0.95. Higher quality means less compression.
#' @return No return value, called for side effects.
#' @examples
#' \dontrun{
#' imsave( face )
#' imsave( face, name = "myimage", path = "path/to/image", format = "jpg" )
#' }
#' @export
im_save = function( im, name, path = getwd(), format = "png", quality = .95 ){
  if( !format %in% c( "jpg", "png", "tiff", "bmp" ) ){
    warning( "Incorrect imaeg format. imsave supports jpg, png, tiff, and bmp." )
    return()
  }
  if( base::missing( name ) ){
    name = deparse( substitute( im ) )
  }
  if( get_n_channel( im ) == 1 ){
    im = rep_channel( im, 3 )
  }
  if( ! imager::is.cimg( im ) && is.array( im ) ){
    im = array2cimg( im )
  }
  if( stringr::str_sub( path, stringr::str_length( path ) ) == "/" ){
    path = stringr::str_sub( path, end = stringr::str_length( path ) - 1 )
  }
  base::dir.create( path, showWarnings = FALSE, recursive = TRUE )
  imager::save.image( clamping( im ), paste0( path, "/", name, ".", format ), quality )
}



#' Convert to grayscale
#'
#' Convert a color image to grayscale.
#'
#' @param im an image (array).
#' @param drop if TRUE (default), returns an image with a single channel,
#' otherwise keep the three channels.
#' @return an array of the grayscaled image
#' @importFrom magrittr %>%
#' @export
#' @examples
#' pplot(face) # color
#' im = im_gray(face)
#' pplot(im) # grayscale
im_gray = function( im, drop = TRUE ){
  if( ! imager::is.cimg( im ) ){
    array2cimg( im ) %>%
      im_gray( drop ) %>%
      cimg2array %>%
      return
  } else {
    if( imager::spectrum( im ) == 1 ){
      return( im )
    } else {
      return( imager::grayscale( im, method = "Luma", drop ) )
    }
  }
}



rep_channel = function( im, n = 3 ){
  return( array( im, c( im_height( im ), im_width( im ), n, 1 ) ) )
}



get_n_channel = function( im ){
  if( imager::is.cimg( im ) ){
    return( imager::spectrum( im ) )
  } else {
    return( dim( im )[ 3 ] )
  }
}



im_width = function( im ){
  if( imager::is.cimg( im ) ){
    return( imager::width( im ) )
  } else {
    return( dim( im )[ 2 ] )
  }
}



im_height = function( im ){
  if( imager::is.cimg( im ) ){
    return( imager::height( im ) )
  } else {
    return( dim( im )[ 1 ] )
  }
}



im_size = function( im ){
  if( imager::is.cimg( im ) ){
    return( c( imager::width( im ), imager::height( im ) ) )
  } else {
    return( unname( dim( im )[ 1:2 ] ) )
  }
}



im_pad = function( im, n, method = "mirror" ){
  if( n == 0 ) return( im )

  if( imager::is.cimg( im ) ){
    im = array2cimg( im_pad( cimg2array( im ), n, method ) )
    return( im )
  }

  w = im_width( im )
  h = im_height( im )

  if( any( n > c( w, h ) ) ){
    warning( "n must be equal or smaller than image width (and height)." )
    return( im )
  }

  # create an empty matrix
  x = ifelse( is.numeric( method ), method, ifelse( method == "mean", mean( im ), 0 ) )
  mat = array( x, c( h + 2 * n, w + 2 * n, dim( im )[ 3:4 ] ) )

  # put the image
  mat[ ( n + 1 ):( n + h ), ( n + 1 ):( n + w ), , ] = im

  # padding
  if( method == "zero" || method == "mean" || is.numeric( method ) ){
    # do nothing
  } else if( method == "repeat" ){
    # top left
    mat[ 1:n, 1:n, , ] = im[ (h-n+1):h, (w-n+1):w, , ]
    # top
    mat[ 1:n, (n+1):(n+w), , ] = im[ (h-n+1):h, 1:w, , ]
    # top right
    mat[ 1:n, (n+w+1):(2*n+w), , ] = im[ (h-n+1):h, 1:n, , ]
    # left
    mat[ (n+1):(n+h), 1:n, , ] = im[ 1:h, (w-n+1):w, , ]
    # right
    mat[ (n+1):(n+h), (n+w+1):(2*n+w), , ] = im[ 1:h, 1:n, , ]
    # bottom left
    mat[ (n+h+1):(2*n+h), 1:n, , ] = im[ 1:n, (w-n+1):w, , ]
    # bottom
    mat[ (n+h+1):(2*n+h), (n+1):(n+w), , ] = im[ 1:n, 1:w, , ]
    # bottom right
    mat[ (n+h+1):(2*n+h), (n+w+1):(2*n+w), , ] = im[ 1:n, 1:n, , ]
  } else if( method == "mirror" ){
    # top left
    mat[ 1:n, 1:n, , ] = im[ n:1, n:1, , ]
    # top
    mat[ 1:n, (n+1):(n+w), , ] = im[ n:1, 1:w, , ]
    # top right
    mat[ 1:n, (n+w+1):(2*n+w), , ] = im[ n:1, w:(w-n+1), , ]
    # left
    mat[ (n+1):(n+h), 1:n, , ] = im[ 1:h, n:1, , ]
    # right
    mat[ (n+1):(n+h), (n+w+1):(2*n+w), , ] = im[ 1:h, w:(w-n+1), , ]
    # bottom left
    mat[ (n+h+1):(2*n+h), 1:n, , ] = im[ h:(h-n+1), n:1, , ]
    # bottom
    mat[ (n+h+1):(2*n+h), (n+1):(n+w), , ] = im[ h:(h-n+1), 1:w, , ]
    # bottom right
    mat[ (n+h+1):(2*n+h), (n+w+1):(2*n+w), , ] = im[ h:(h-n+1), w:(w-n+1), , ]
  }

  im = mat
  return( im )
}



im_crop = function( im, margin ){
  if( imager::is.cimg( im ) ){
    im = im %>% cimg2array %>% im_crop( margin ) %>% array2cimg
    return( im )
  }
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
  width = im_width( im )
  height = im_height( im )
  im = im[ (1 + top):(height - bottom), (1 + left):(width - right), , , drop = FALSE ]
  return( im )
}



im_combine = function( im1, im2, y = 0, x = 0, background = 1 ){
  cc = max( get_n_channel( im1 ), get_n_channel( im2 ) )
  h = max( im_height( im1 ), y + im_height( im2 ), im_height( im2 ), - y + im_height( im1 ) )
  w = max( im_width( im1 ), x + im_width( im2 ), im_width( im2 ), - x + im_width( im1 ) )
  im = array( rep( background, each = h * w, times = cc ), dim = c( h, w, cc, 1 ) )

  y1 = ifelse( y < 0, -y, 0 ) + 1
  y2 = ifelse( y < 0, 0, y ) + 1
  x1 = ifelse( x < 0, -x, 0 ) + 1
  x2 = ifelse( x < 0, 0, x ) + 1
  im[ y1:( y1 + im_height( im1 ) - 1 ), x1:( x1 + im_width( im1 ) - 1 ), 1:cc, ] = im1
  im[ y2:( y2 + im_height( im2 ) - 1 ), x2:( x2 + im_width( im2 ) - 1 ), 1:cc, ] = im2

  return( im )
}



#' Resize image
#'
#' Resize image by a single scale factor.
#'
#' @param im an image (array).
#' @param scale scale factor
#' @return an array of the scaled image.
#' @export
#' @examples
#' dim(face) # original size
#' im = im_scale(face, 0.5)
#' dim(im) # resized to half of its original size
im_scale = function( im, scale = 1 ){
  if( imager::is.cimg( im ) ){
    im = imager::imresize( im, scale )
    return( im )
  }
  im = cimg2array( imager::imresize( array2cimg( im ), scale ) )
  return( im )
}



box_blur = function( im, r ){
  if( r < 1 ){
    return( im )
  }

  if( get_n_channel( im ) != 1 ){
    warning( "the number of color channel must be 1." )
    return( invisible( NULL ) )
  }

  L = 2 * r + 1
  width = im_width( im )
  height = im_height( im )

  im = im_pad( im, r, method = "mirror" )

  out = array( 0.0, dim( im ) )
  cumsum = rowSums( im[ , 1:(2*r), , ] )
  cumsum = cumsum + im[ ,r + 1 + r, , ]
  out[ , r + 1, , ] = cumsum / L
  for( i in ( r + 2 ):( width + r ) ){
    cumsum = cumsum + im[ ,i + r, , ] - im[ ,i - r - 1, , ]
    out[ , i, , ] = cumsum / L
  }

  im = out
  cumsum = colSums( im[ 1:(2*r), , , ] )
  cumsum = cumsum + im[ r + 1 + r, , , ]
  out[ r + 1, , , ] = cumsum / L
  for( i in ( r + 2 ):( height + r ) ){
    cumsum = cumsum + im[ i + r, , , ] - im[ i - r - 1, , , ]
    out[ i, , , ] = cumsum / L
  }

  out = im_crop( out, r )
  return( out )
}



box_variance = function( im, radius ){
  variance = box_blur( im^2, radius ) - box_blur( im, radius )^2
}



guided_filter = function( p, radius, epsilon = 0.1, I = p ){
  p = im_gray( p )

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



get_gauss_filter = function( sd, radius = round( 2.5 * sd ) ){
  if( sd < 0.2 ){
    return( NULL )
  }
  L = 2 * radius + 1
  matx = matrix( stats::dnorm( 1:L, mean = radius + 1, sd = sd ), nrow = L, ncol = L, byrow = FALSE )
  maty = matrix( stats::dnorm( 1:L, mean = radius + 1, sd = sd ), nrow = L, ncol = L, byrow = TRUE )
  mat = matx * maty
  mat = mat / sum( mat )
  return( array( mat, c( L, L, 1, 1 ) ) )
}



maximum_filter = function( im, radius, pad.method = "mirror" ){
  im = im_pad( im, radius, method = pad.method )
  im2 = im
  for( cy in ( 1 + radius ):( im_height( im ) - radius ) ){
    for( cx in ( 1 + radius ):( im_width( im ) - radius ) ){
      im2[ cy, cx, 1, 1 ] = max(
        im[ ( cy - radius ):( cy + radius ), ( cx - radius ):( cx + radius ), 1, 1 ]
      )
    }
  }
  im2 = im_crop( im2, radius )
  return( im2 )
}



im_conv = function( im, kernel, pad.method = "mirror", use_luminance = TRUE ){
  if( is.null( kernel ) ){
    return( im )
  }
  if( use_luminance ){
    im = im_gray( im )
  }
  is_cimg = TRUE
  if( ! imager::is.cimg( im ) ){
    im = array2cimg( im )
    is_cimg = FALSE
  }
  if( ! imager::is.cimg( kernel ) ){
    kernel = array2cimg( kernel )
  }
  npad = floor( max( dim( kernel )[ 1:2 ] ) / 2 )
  im = imager::convolve( im_pad( im, npad, method = pad.method ), kernel )
  im = imager::crop.borders( im, nPix = npad )
  if( ! is_cimg ){
    im = cimg2array( im )
  }
  return( im )
}



decompose = function( im, Depth, log_epsilon = 0.0001, filter_epsilon = 0.01 ){
  if( Depth < 1 ){
    return( im )
  }
  if( imager::is.cimg( im ) ){
    im = cimg2array( im )
  }

  L = im_gray( im )
  L = log( L + log_epsilon )

  # L0 = L
  # Lk = guided_filter( Lk-1, filter_epsilon, 2^k ) (k=1~n)
  # Dk = Lk-1 - Lk
  # recon = ∑(Dk)[k=1~n] + Ln
  N = floor( log2( min( im_size( L ) ) ) )
  L_k_minus_1 = guided_filter( L, 2^1, filter_epsilon ) # L1
  D_k = L - L_k_minus_1 # D1
  D = list( D_k )
  N = min( N, Depth )
  if( N > 1 ){
    for( k in 2:N ){
      L_k = guided_filter( L_k_minus_1, 2^k, filter_epsilon )
      D_k = L_k_minus_1 - L_k
      D = c( D, list( D_k ) )
      if( k == N ){
        Low_residual = list( residual = L_k )
      } else {
        L_k_minus_1 = L_k
      }
    }
    names( D ) = paste0( "D", sprintf( paste0( "%0", nchar( N ), "d" ), 1:N ) )
    dec = c( D, Low_residual )
  } else {
    dec = list( D1 = D_k, residual = L_k_minus_1 )
  }

  return( dec )
}



reconstruct = function( dec, log_epsilon = 0.0001 ){
  N = length( dec ) - 1
  recon = array( 0, dim( dec[[ 1 ]] ) )
  for( i in 1:N ){
    recon = recon + dec[[ i ]]
  }
  recon = recon + dec[[ N + 1 ]]
  recon = exp( recon ) - log_epsilon
  return( recon )
}



get_attenuated = function( im, smoothing ){
  dec = decompose( im, smoothing )
  strength = 0.05
  for( i in 1:smoothing ){
    dec[[ i ]] = ifelse( i == 1, 0, strength ) * dec[[ i ]]
  }
  im = reconstruct( dec )
  return( im )
}



get_residual = function( im, Depth, log_epsilon = 0.0001, filter_epsilon = 0.01 ){
  if( Depth < 1 ){
    return( im )
  }
  if( imager::is.cimg( im ) ){
    im = cimg2array( im )
  }

  L = im_gray( im )
  L = log( L + log_epsilon )

  N = min( Depth, floor( log2( min( im_size( L ) ) ) ) )
  L_k_minus_1 = guided_filter( L, 2^1, filter_epsilon ) # L1
  if( N > 1 ){
    for( k in 2:N ){
      L_k = guided_filter( L_k_minus_1, 2^k, filter_epsilon )
      if( k == N ){
        residual = L_k
      } else {
        L_k_minus_1 = L_k
      }
    }
  } else {
    residual = L_k_minus_1
  }

  residual = exp( residual ) - log_epsilon
  return( residual )
}



#' Plot an image
#'
#' @param im an image (array).
#' @param rescale logical. if TRUE, pixel values are rescaled so that they are in \[0,1\].
#' @return No return value, called for side effects.
#' @export
#' @examples
#' pplot(face)
pplot = function( im, rescale = FALSE ){
  old.par = graphics::par(no.readonly = TRUE )
  on.exit( graphics::par( old.par ) )

  if( ! imager::is.cimg( im ) ){
    im = array2cimg( im )
  }
  if( dim( im )[ 4 ] == 2 ){
    im = imager::R( im )
  } else if( dim( im )[ 4 ] > 3 ){
    im = imager::imappend(
      list( imager::R( im ), imager::G( im ), imager::B( im ) ), axis = "c" )
  }
  graphics::par( mar = c( 0.5, 0.5, 0.5, 0.5 ) )
  if( rescale ){
    graphics::plot(
      im, axes = FALSE, xlab = "", ylab = "", interpolate = FALSE, rescale = TRUE )
  } else {
    graphics::plot(
      clamping( im ), axes = FALSE, xlab = "", ylab = "", interpolate = FALSE, rescale = FALSE )
  }
}



#' Apply a sketch effect on an image
#'
#' @param im an image (array).
#' @param style numeric (integer). Either 1 (edge-focused) or 2 (shading preserving)
#' @param lineweight numeric (integer). Strength of lines.
#' @param smoothing numeric (integer). Smoothness of image texture.
#' @param contrast numeric (integer). Adjusts the image contrast.
#' @param gain numeric (double). Can be used to reduce noise in dim regions.
#' @return an array of the sketched image.
#' @export
#' @examples
#' im = sketch(face, style = 1, lineweight = 1, smoothing = 1)
#' pplot(im)
#'
#' \dontrun{
#' im = im_load("path/to/your/image.jpg")
#' pplot(im)
#' }
sketch = function( im, style = 1, lineweight = 1, smoothing = 1,
                   contrast = ifelse( style == 1, 8, 3 ), gain = 0.1 ){
  im = im_gray( im )
  im = raise_bottom( im, gain )
  N = floor( log2( min( im_size( im ) ) ) )
  if( smoothing > N ){
    warning( paste0( "smoothing exceeded the maximum possible value for the input image. smoothing = ",
                     N, " was used instead.") )
    smoothing = N
  }
  if( smoothing >= 1 ){
    # im = get_attenuated( im, smoothing )
    im = get_residual( im, smoothing )
  }

  if( style == 1 ){
    kernel = get_gauss_filter( lineweight )
    temp = im_conv( im, kernel, pad.method = "mirror", use_luminance = TRUE )
  } else if( style == 2 ){
    temp = maximum_filter( im, lineweight, "mirror" )
  }

  im2 = clamping( im / temp )

  return( base::invisible( im2^contrast ) )
}



#' Apply the Canny edge detection
#'
#' This is a wrapper of cannyEdges() function in the imager package.
#' t1 and t2 determines the edge threshold.
#' if the threshold parameters are missing, they are determined automatically using a k-means heuristic.
#' alpha parameter adjusts the automatic thresholds up or down.
#' The edge detection is based on a smoothed image gradient (set by the sigma parameter).
#' If smoothing parameter is set, high frequency noise is removed before applying the Canny edge detection.
#'
#' @param im an image.
#' @param t1, threshold for weak edges (if missing, both thresholds are determined automatically).
#' @param t2 threshold for strong edges.
#' @param alpha threshold adjustment factor (default 1).
#' @param sigma degree of smoothing image gradient.
#' @param smoothing numeric (integer). Smoothness of image texture.
#' @return an array of the edge image.
#' @export
#' @examples
#' im = canny(face)
#' pplot(im)
canny = function( im, t1, t2, alpha = 1, sigma = 2, smoothing = 0 ){
  im = im_gray( im )
  N = floor( log2( min( im_size( im ) ) ) )
  if( smoothing > N ){
    warning( paste0( "smoothing exceeded the maximum possible value for this image. smoothing = ",
                     N, " was used instead.") )
    smoothing = N
  }
  if( smoothing >= 1 ){
    im = get_residual( im, smoothing )
  }

  im2 = imager::cannyEdges( array2cimg( im ), t1, t2, alpha = 1, sigma = 2 )
  im2 = cimg2array( 1 - im2 )

  return( base::invisible( im2 ) )
}



#' Create multiple sketches at once and combine them into a single image
#'
#' It is often necessary to find optimal sketch style parameters for your task.
#' With this function, you can easily compare the effects of different style parameters.
#'
#' @param im an image.
#' @param style numeric (integer). Either 1 (edge-focused) or 2 (smooth gradient)
#' @param weight_levels numeric (integer). a vector of lineweight values
#' @param smooth_levels numeric (integer). a vector of smoothing values
#' @param contrast numeric (integer). Adjusts the image contrast.
#' @param verbose If TRUE (default), progress information is displayed in the Console.
#' @return an array of the sketched image.
#' @export
#' @examples
#' \donttest{
#' im = survey(face)
#' pplot(im)
#'
#' im = survey(face, style = 1, weight_levels = c(1, 2), smooth_levels = c(0, 2), contrast = 9)
#' pplot(im)
#' }
survey = function( im, style = 1,
                   weight_levels = c(1, 3, 5), smooth_levels = c(0, 2, 4), contrast, verbose = TRUE ){
  if( verbose ){
    N = length( smooth_levels ) * length( weight_levels )
    n = 1
    cat( "Sketching 9 images: " )
  }

  if( base::missing( contrast ) ){
    contrast = c( 7, 3 )
  } else if( length( contrast ) == 1 ){
    contrast = rep( contrast, 2 )
  }

  imgs = NULL
  for( s in 1:length( smooth_levels ) ){
    for( t in 1:length( weight_levels ) ){
      if( verbose ){
        cat( paste0( n, " ") )
      }
      im2 = sketch( im, style, weight_levels[ t ], smooth_levels[ s ], contrast[ style ] )
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

  return( base::invisible( imgs ) )
}


