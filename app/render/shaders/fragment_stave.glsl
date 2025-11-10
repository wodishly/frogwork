#version 410

precision highp sampler2D;

in vec2 v_uv;
out vec4 f_color;

uniform sampler2D u_texture;
uniform float u_time;

void main() {
    vec4 value = vec4(vec3(1.0), texture(u_texture, v_uv).r * u_time / 2.0);
    f_color = value * vec4(1.0);
}
