#version 410

precision highp sampler2D;

in vec2 v_uv;
out vec4 f_color;

uniform sampler2D u_texture;

void main() {
    // vec3 color = texture(u_texture, v_uv).rgb;
    f_color = vec4(v_uv, 1.0, 1.0);
}
