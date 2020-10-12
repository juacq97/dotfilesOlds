

line_width = 5
top_left_x = 20
top_left_y = 20
rec_width = 100
rec_height = 50
red = 1
green = 0
blue = 0
alpha = 1

-- Draw it.
cairo_set_line_width (cr, line_width)
cairo_rectangle (cr, top_left_x, top_left_y, rec_width, rec_height)
cairo_set_source_rgba (cr, red, green, blue, alpha)

