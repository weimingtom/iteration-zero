
class Viewport(object):
	def __init__(self, engine, screen_rect):
		self.screen_x, self.screen_y, self.screen_w, self.screen_h = screen_rect
		self.draw = engine.getGraphics().drawImage
		self.position = 0,0

	def draw_level(self,level):
		w_x,w_y = self.position

		L = level
		tw,th = level.TILE_SIZE
		w_x -= (self.screen_w/2)/tw
		w_y -= (self.screen_h)/th

		w_width   = (self.screen_w/2)/tw
		w_hheight = (self.screen_h)/th
		
		for row in range(w_width):
			w_tx = w_x 
		dx = tw

		

