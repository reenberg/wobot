#include <evolution/Resource.hpp>

#include <iostream>


const double velocity = 20; // cm/sec
const double acceleration = 20; // cm/sec^2
const double angular_velocity = 0.5; // radians/sec
const double angular_acceleration = M_PI/2.0; // radians/sec^2

/*
 * The robot interfaces
 * Keep your interface handles here.
 */
Evolution::ResourceManager *resource_manager;  // Just an example
Evolution::IResourceContainer *resource_container;
Evolution::IDriveSystem *driver;
Evolution::IAvoidance *avoid;

/*
 * Initializes the needed interfaces.
 * The interface handles are stored as global variables.
 */
bool initialize_interfaces()
{
    Evolution::Result result;
    bool success = true;

    // Get the ERSP resource manager
    resource_manager = new Evolution::ResourceManager( NULL, &result ); 
    if (result != Evolution::RESULT_SUCCESS) {
        std::cerr << "Cannot create a resource manager." << std::endl;
        success = false;
    }

    result = resource_manager->get_resource_container( 0, &resource_container );
    if (result != Evolution::RESULT_SUCCESS) {
        std::cerr << "Cannot create a resource container." << std::endl;
        success = false;
    }


    result = resource_container->obtain_interface(Evolution::NO_TICKET, "drive", 
            Evolution::IDriveSystem::INTERFACE_ID, (void**) &driver);
    if (result != Evolution::RESULT_SUCCESS) {
        std::cerr << "Cannot create a drive system." << std::endl;
        success = false;
    }

    result = resource_container->obtain_interface(Evolution::NO_TICKET, "avoidance", 
           Evolution::IAvoidance::INTERFACE_ID, (void**) &avoid);
    avoid->disable_avoidance(Evolution::NO_TICKET);
    if (result != Evolution::RESULT_SUCCESS) {
        std::cerr << "Cannot create avoidance." << std::endl;
        success = false;
    }

    return success;
}

/*
 * Shutdown the initialized interfaces.
 */
bool shutdown_interfaces()
{
//    Evolution::Result result;
    bool success = true;


    // Deallocate the Resource Manager
    if (resource_manager != NULL) delete resource_manager;

    return success;
}

/*
 * The main program.
 */
int main(int argc, char *argv[]) 
{
    //Evolution::Result result;

    // Initialize interfaces
    const bool init_success = initialize_interfaces();
    if (!init_success) return -1;

    for(int i=0;4>i;i++){
    driver->move_delta(Evolution::NO_TICKET, NULL, 0, 100, velocity, acceleration);
    sleep(6);
    driver->turn_delta(Evolution::NO_TICKET, NULL, 0, angular_acceleration*0.95, angular_velocity, angular_acceleration);
    sleep(4);
}
    //driver->stop();

/*
     * Place your program here.
     */

    // Shutdown interfaces
    const bool shutdown_success = shutdown_interfaces();
    if (!shutdown_success) return -1;

    // And we're done
    return 0;
}
