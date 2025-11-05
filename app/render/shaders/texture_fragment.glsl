#version 410

precision highp sampler2D;

in vec2 v_uv;
in vec3 v_normal;
out vec4 pc_fragColor;

uniform sampler2D u_texture;
uniform float u_time;

void main() {
    float s = -sqrt(3) / 3;
    vec3 light_dir = vec3(s * cos(u_time), s, s * sin(u_time));

    float ambient = 0.1;
    float diffuse = max(0, dot(-normalize(light_dir), v_normal));
    float lighting = ambient + diffuse;
    vec3 color = texture(u_texture, v_uv).rgb;

    pc_fragColor = vec4(color * lighting, 1.0);
}
