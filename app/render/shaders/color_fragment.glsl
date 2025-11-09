#version 410

in vec4 v_position;
out vec4 f_color;

void main() {
    vec2 xz = floor(mod(vec2(v_position.x, v_position.z + 1.0), 2.0));
    float value = xz.x + xz.y == 1.0 ? 1.0 : 0.0;
    f_color = vec4(vec3(value), 1.0);
}
