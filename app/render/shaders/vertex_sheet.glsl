#version 410

in vec3 position;
out vec2 v_uv;

void main() {
        v_uv = (position.xy / 2.0) + 1.0;
        gl_Position = vec4( position, 1.0 );
}