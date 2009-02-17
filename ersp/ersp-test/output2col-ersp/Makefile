
# The ersp package config file must be in the PKG_CONFIG_PATH, e.g.
# export PKG_CONFIG_PATH=$PKG_CONFIG_PATH:/opt/evolution_robotics/lib/pkgconfig

# Compile and link flags
CXX      =  g++
CXXFLAGS = `pkg-config ersp --cflags` -Wall -g
LIBS     = `pkg-config ersp --libs` -I/opt/jdk1.5.0_08/include
#JNI      =  -I/opt/jdk1.5.0_08/include -I/opt/jdk1.5.0_08/include/linux -L/opt/jdk1.5.0_08/lib -L/opt/jdk1.5.0_08/jre/lib/i386/client
#JVM      = -ljvm

#working
#g++ -g -I/opt/jdk1.5.0_08/include -I/opt/jdk1.5.0_08/include/linux -L/opt/jdk1.5.0_08/lib -L/opt/jdk1.5.0_08/jre/lib/i386/client -ljvm Sample2.cpp

# Main executable
# EXECUTABLE = testEasy
#EXECUTABLE = charge
EXECUTABLE = main

# Files
# SOURCES = IDriveEasy.cpp testEasy.cpp
#SOURCES = charge.cpp
#SOURCES = main.cpp fuzzy.cpp ISensorEasy.cpp IDriveEasy.cpp
SOURCES = main.cpp ISensorEasy.cpp
OBJECTS = $(patsubst %.cpp, %.o, $(SOURCES))

## Compile everything ##
all: $(OBJECTS)
	$(CXX) $(CXXFLAFS) $(LIBS) $(OBJECTS) -o $(EXECUTABLE)

# Compilation.
%.o : %.cpp
	$(CXX) $(CXXFLAGS) $(JNI) -c $^

# Cleaning up
clean :
	rm -f *.o *.lo *.la *.so *~ $(EXECUTABLE)
