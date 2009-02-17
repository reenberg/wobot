
# The ersp package config file must be in the PKG_CONFIG_PATH, e.g.
# export PKG_CONFIG_PATH=$PKG_CONFIG_PATH:/opt/evolution_robotics/lib/pkgconfig

# Compile and link flags
CXX      =  g++
CXXFLAGS = `pkg-config ersp --cflags` -Wall
LIBS     = `pkg-config ersp --libs`

# Main executable
EXECUTABLE = skabelon

# Files

## Compile everything ##

square-ersp: $(patsubst %.cc, %.o, square-ersp.cc)
	$(CXX) $(LIBS) $(patsubst %.cc, %.o, square-ersp.cc) -o square-ersp


# Compilation.
%.o : %.cc
	$(CXX) $(CXXFLAGS) -c $^ 

# Cleaning up
clean :
	rm -f *.o *.lo *.la *.so *~ $(EXECUTABLE)
