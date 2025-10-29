#version 410

in vec2 position;
out vec2 v_uv;

void main() {
        v_uv = (position / 2.0) + 1.0;
        gl_Position = vec4( position, 0.0, 1.0 );
}
