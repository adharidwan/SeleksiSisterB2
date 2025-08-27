#include <string.h>

#define SECRET_TOKEN "HUTAOTHEGOAT"


int is_protected_route(const char *filename) {
    if (filename != NULL && strcmp(filename, "secret.html") == 0) {
        return 1; // Ya, ini adalah rute yang dilindungi
    }
    return 0; 
}

int check_authentication(const char *request_buffer) {
    const char *auth_header_prefix = "Authorization: Bearer ";
    char *header_location = strstr(request_buffer, auth_header_prefix);

    if (header_location) {
        const char *provided_token = header_location + strlen(auth_header_prefix);
        if (strncmp(provided_token, SECRET_TOKEN, strlen(SECRET_TOKEN)) == 0) {
            return 1; 
        }
    }
    
    return 0; 
}